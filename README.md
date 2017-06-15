# slack-sentimental-bot
A Slack bot for sentiment analysis, for the course MCS at HEIG-VD.

## Features
- Sentimental analysis
- Change the name of the bot
- Get a list of all users

## Getting started
Open your favorite terminal and typ:
- `cd /PATH/TO/SENTIBOT/`
- `echo '"YOUR_TOKEN".' > priv/token.txt`
- `rebar3 auto`
- `application:start(sentibot).`

You will need [rebar3](https://www.rebar3.org/v3/docs).

If you don't have a _token_ yet, please look [here](https://api.slack.com/bot-users).

And don't forget to add your bot to desired channels ;)

## Live exemple
![live exemple](res/live-exemple.png)

## Documentation
More informations about the project can be found [here](DOC.md).

## Credits
- Champion Patrick
- Henocq Raphaël
- Peretti Christophe
