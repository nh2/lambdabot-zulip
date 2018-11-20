# lambdabot-zulip

A [`lambdabot`](https://wiki.haskell.org/Lambdabot)-like bot for [Zulip](https://zulipchat.com/).

Can evaluate Haskell expressions and show their types.

### Screenshot

![Screenshot of the bot in action](images/evaluation-screenshot.png)


## Usage

Run the `lambdabot-zulip-server` executable to start the bot.

It reads a `settings.yaml` in the working directory (or passed via command line).

See [`example-settings/settings.yaml`](example-settings/settings.yaml) for an example.
You have to provide Zulip API credentials, and streams (channels) the bot should be active on.
