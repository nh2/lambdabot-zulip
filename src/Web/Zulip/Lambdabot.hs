{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

-- | A lambdabot for the Zulip web chat.
module Web.Zulip.Lambdabot where

import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Data.Char (isDigit)
import           Data.List (stripPrefix)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml.Aeson (FromJSON, ToJSON)
import           Data.Yaml.Config
import           GHC.Generics
import           Language.Haskell.Interpreter (runInterpreter, InterpreterError(..), GhcError(errMsg))
import           Mueval.ArgsParse (typeOnly, interpreterOpts)
import           Mueval.Interpreter (interpreter)
import qualified Options.Applicative as Opts
import           Say (say)
import           Text.ParserCombinators.ReadP (ReadP, readP_to_S, char, munch1, skipSpaces)
import           Web.HZulip
import           System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))


-- Adapted from https://hackage.haskell.org/package/mueval-0.9.3/docs/src/Mueval-Interpreter.html#printInterpreterError

-- | Removes uninteresting parts of error messages.
-- Example:
-- > dropErrorLinePosition "<interactive>:1:1: error: Variable not in scope: help"
-- is
-- > "error: Variable not in scope: help"
dropErrorLinePosition :: String -> String
dropErrorLinePosition e =
  case parseReturnRest interactiveErrorPrefixParser e of
    Just rest -> rest
    Nothing -> e -- if the parse fails we fallback on printing the whole error
  where
    interactiveErrorPrefixParser :: ReadP ()
    interactiveErrorPrefixParser = do
      pure ()
      <* char '<' <* munch1 (/= '>') <* char '>' <* char ':' -- e.g. <interactive>:
      <* munch1 isDigit <* char ':' -- line
      <* munch1 isDigit <* char ':' -- column
      <* skipSpaces

    parseReturnRest :: ReadP () -> String -> Maybe String
    parseReturnRest parser input =
      case readP_to_S parser input of
        [] -> Nothing
        (_dropped, rest):_ -> Just rest


-- | Turns a @hint@ `InterpreterError` into our simple error type.
formatInterpreterError :: InterpreterError -> EvalErrors
formatInterpreterError err = case err of
  WontCompile errors -> EvalErrors $ map (dropErrorLinePosition . errMsg) errors
  UnknownError str -> EvalErrors [str]
  NotAllowed str -> EvalErrors [str]
  GhcException str -> EvalErrors [str]


-- | List of errors output by the bot when a query string couldn't be run
-- or didn't succeed (such as GHC compiler error messages).
-- Returns a list so that they can be displayed as independent code blocks
-- if desired.
newtype EvalErrors = EvalErrors [String]
  deriving (Eq, Ord, Show)

-- | Result for input like @1 + 1@.
data EvalSuccess = EvalSuccess
  { evalSuccessValue :: String
  , evalSuccessType :: String
  } deriving (Eq, Ord, Show)

-- | Result for input like @:type 1 + 1@.
newtype TypecheckSuccess = TypecheckSuccess String -- ^ type
  deriving (Eq, Ord, Show)


-- | Result of the bot trying to run one string.
data LambdabotResult
  = ResultErrors EvalErrors
  | ResultEvalSuccess EvalSuccess
  | ResultTypecheckSuccess TypecheckSuccess
  deriving (Eq, Ord, Show)


-- | Evaluates an expression.
-- Returns the value and type on success, or the
-- error message(s) on failure.
evalCommand :: String -> IO LambdabotResult
evalCommand command = do
  let (typecheckOnly, expr)
        | Just e <- stripPrefix ":type " command = (True, e)
        | Just e <- stripPrefix ":t " command = (True, e)
        | otherwise = (False, command)
  case interpreterOpts ["--expression", expr] of
    Left (_success, output) ->
      return $ ResultErrors $ EvalErrors [output]
    Right opts -> do
      r <- runInterpreter (interpreter opts{ typeOnly = typecheckOnly })
      return $ case r of
        Left err -> ResultErrors $ formatInterpreterError err
        Right (_expr, exprType, resultVal)
          | typecheckOnly -> ResultTypecheckSuccess $ TypecheckSuccess exprType
          | otherwise -> ResultEvalSuccess $ EvalSuccess resultVal exprType


-- | Turns the result of one query string to the bot into a nicely formatted
-- Zulip message.
formatResult :: FloodProtectionOutputLength -> LambdabotResult -> Text
formatResult (FloodProtectionOutputLength maxOutputLength) result = reply
  where
    floodProtect str =
      if length str > maxOutputLength
        then take maxOutputLength str ++ "..."
        else str
    reply = case result of
      ResultErrors (EvalErrors errs) ->
        T.concat $ flip map errs $ \err -> T.unlines
          [ ":cross_mark:"
          , "```"
          , T.pack (floodProtect err)
          , "```"
          ]
      ResultTypecheckSuccess (TypecheckSuccess typ) ->
        ":check_mark: `:: " <> T.pack typ <> "`"
      ResultEvalSuccess EvalSuccess{ evalSuccessValue = val
                                   , evalSuccessType = typ } ->
        T.unlines
          [ ":check_mark:"
          , "```"
          , T.pack (floodProtect val)
          , "```"
          , "`:: " <> T.pack typ <> "`"
          ]


newtype FloodProtectionOutputLength = FloodProtectionOutputLength Int
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


-- | Settings for the bot.
data Settings = Settings
  { zulipUserName :: Text -- ^ Example: @lambdabot-bot@myteam.zulipchat.com@
  , zulipApiKey :: Text -- ^ Example: @abc123abc123abc123abc123abc123ab@
  , zulipBaseUrl :: Text -- ^ Example: @https://myteam.zulipchat.com/api/v1@
  , botStreams :: [Text] -- ^ Example: @["haskell"]@
  , floodProtectionOutputLength :: FloodProtectionOutputLength -- ^ Outputs beyond this length will be @...@'d.
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


-- | Main entry point for the Zulip lambdabot.
runZulipLambdabot :: Settings -> IO ()
runZulipLambdabot settings = do
  let Settings
        { zulipUserName = botUserName
        , zulipApiKey = apiKey
        , zulipBaseUrl = baseUrl
        , botStreams = botStreamsList
        , floodProtectionOutputLength
        } = settings

  let botStreamsSet = Set.fromList botStreamsList

  options <- do
    o <- zulipOptions botUserName apiKey
    return o{ clientBaseUrl = baseUrl }

  withZulip options $ do
    -- Show current subscriptions
    subscriptions <- getSubscriptions

    when (Set.fromList subscriptions /= botStreamsSet) $ do
      -- Remove undesired stream subscriptions
      removeSubscriptions [ s | s <- subscriptions, s `Set.notMember` botStreamsSet]
      -- Subscribe to desired streams
      addSubscriptions botStreamsList

    say $ "Listening to streams: " <> T.intercalate ", " (Set.toList botStreamsSet)

    me <- getProfile
    let myUserId = profileUserId me

    -- Listening for events works with a callback based API:
    onNewMessage $ \msg -> do
      let sender = messageSender msg :: User
          _senderName = userFullName sender
          subject = messageSubject msg

      -- Private messages one sends also appear as message events.
      -- Ignore them to avoid generating an infinite stream of messages.
      if userId sender == myUserId
        then liftIO $ putStrLn "Got message from myself, ignoring"
        else
          -- Only answer requests that go to the right stream.
          case messageDisplayRecipient msg of
            Right _user -> do
              liftIO $ putStrLn "Got direct message to user, ignoring"
            Left stream | stream `Set.member` botStreamsSet -> do

              -- Only answer requests that start with the evaluation symbol.
              case T.stripPrefix "> " (messageContent msg) of
                Nothing -> return ()
                Just input -> do
                  -- Run interpreter
                  result <- liftIO $ evalCommand (T.unpack input)
                  let format = formatResult floodProtectionOutputLength
                  -- Post reply
                  _msgId <- sendStreamMessage stream subject (format result)
                  return ()

            Left _stream -> do
              liftIO $ putStrLn "Got message to non-'haskell' stream, ignoring"


-- | Command line arguments for this program.
data Args = Args
  { argsSettingsFile :: FilePath -- ^ Path to the settings YAML file
  } deriving (Eq, Ord, Show)


-- | Command line argument parser for this program.
argsParser :: Opts.Parser Args
argsParser =
  Args
    <$> Opts.strOption
          ( Opts.long "config-file"
         <> Opts.metavar "FILE"
         <> Opts.value "settings.yaml"
         <> Opts.help "Path to configuration file"
          )


-- | Parses command line arguments for this program.
parseCommandLine :: IO Args
parseCommandLine = Opts.execParser $ Opts.info (Opts.helper <*> argsParser) Opts.fullDesc


runServer :: IO ()
runServer = do
  hSetBuffering stdout LineBuffering

  Args{ argsSettingsFile } <- parseCommandLine
  settings <- loadYamlSettings [argsSettingsFile] [] useEnv

  runZulipLambdabot settings
