# Documentation
TODO

## Supervision Tree
Like any other OTP application, our bot is composed of _supervisors_ and _workers_ that are organized into a supervision tree:

TODO

This tree is composed of those modules:

| Module | Kind | OTP Behavior | Description |
| ------ | ---- | ------------ | ----------- |
| `sentibot_app` | - | `application` | Entry point |
| `sentibot_sup` | supervisor | `supervisor` | Top level supervisor |
| `sb_sentimental` | worker | `gen_server` | Sentimental analysis |
| `sb_database` | worker | `gen_server` | Store users's sentiments |
| TODO |  |  |  |
