# Documentation
1. Supervision Tree
2. Modules
3. Protocol
4. Dependencies

## Supervision Tree
Like any other OTP application, our bot is composed of _supervisors_ and _workers_ that are organized into a supervision tree:

- `sentibot_app`
  - `sentibot_sup`
    - `sb_sentimental`
    - `sb_database`
    - `sb_slacker`
    - `sb_rtm`

## Modules
The supervision tree is composed of those modules:

| Module | Kind | OTP Behavior | Description |
| ------ | ---- | ------------- | ----------- |
| `sentibot_app` | - | `application` | Entry point |
| `sentibot_sup` | supervisor | `supervisor` | Top level supervisor |
| `sb_sentimental` | worker | `gen_server` | Sentimental analysis |
| `sb_database` | worker | `gen_server` | Store users's sentiments |
| `sb_slacker` | worker | `gen_server` | Connect to the Slack team |
| `sb_rtm` | worker | `websocket_client_handler` | _Real-Time Messaging_ |

## Protocol

| Module | Request | Reply | Description |
| ------ | ------- | ----- | ----------- |
| `sb_sentimental` | `{message, Msg}` | `[Sentiment]` | Return sentiments found into `Msg :: string()` as a list of `Sentiment :: string()` |
| `sb_database` | `{get, User}` | `{User, [Sentiment]}` | Get a list of `Sentiment :: string()` for the user `User :: string()` |
| `sb_database` | `{set, User, [Sentiment]}` | `ok` | Set a list of `Sentiment :: string()` for the user `User :: string()` |
| `sb_database` | `all` | `[{User, [Sentiment]}]` | Return a list of all users with their sentiments, in a tuple `{User :: string(), [Sentiment :: string()]}` |
| TODO |  |  |  |

## Dependencies
- [slacker](https://github.com/julienXX/slacker) - Erlang Slack REST API wrapper
- [lager](https://github.com/erlang-lager/lager) - A logging framework for Erlang/OTP
- [websocket_client](https://github.com/jeremyong/websocket_client) - Erlang websocket client (ws and wss supported)
