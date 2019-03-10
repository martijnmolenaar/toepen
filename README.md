# Toepen gameplay-simulator in R

### installation and running

Install R via [CRAN](https://cran.r-project.org/).

Make sure the R-library `jsonlite` is installed:

```
R
install.packages("jsonlite")
q()
```

Run `toepen.r` in via terminal:

`Rscript toepen.r`

### settings in `toepen.r`

variables to set:
`time_delay`, time delay in seconds between turns
`max_points`, game is lost with this amount of points

game is initialized by `initializeGame()`. Optons are:
 * `numberOfPlayers`: integer
 * `player_names`: vector of strings containing player names (default is player1, player2, ...)
 * `GamePlay`: vector of strings with length `numberOfPlayeres`, containing the following options:
    + `"random_open"`: random playing bot-player with open cards
    + `"random"`: random playing bot-player with closed cards
    + `"manual"`: manual player with interaction via terminal
    + *`string with terminal command`*, for example `"Rscript randomAI.r -f player3.txt"`
    
### Interaction with `toepen.r`

When a player's `GamePlay` is set to a terminal command, `toepen.r` returns a file called `player1.txt` for player1, etc. 
This file is formatted in JSON-style and contains info about the available turn-choices, player deck and game:
```
{
  "choices": ["nieuwe kaarten", "speel"],
  "values": ["J", "Q", "K", "A", "7", "8", "9", "10"],
  "suits": ["D", "C", "H", "S"],
  "players": 3,
  "player_names": ["Martijn", "Wouter", "Ralph"],
  "player_points": {
  ...
```  
*(see bottom of the page for full example)*

The called program should return the same file, with `choices` set to the proposed gameplay choice. 
As an example program, `randomAI.r` was added to illustrate this.

full example of `player1.txt`:

```
{
  "choices": ["nieuwe kaarten", "speel"],
  "values": ["J", "Q", "K", "A", "7", "8", "9", "10"],
  "suits": ["D", "C", "H", "S"],
  "players": 3,
  "player_names": ["Martijn", "Wouter", "Ralph"],
  "player_points": {
    "Martijn": {
      "points": 0,
      "dealer": 1,
      "dead": 0
    },
    "Wouter": {
      "points": 0,
      "dealer": 0,
      "dead": 0
    },
    "Ralph": {
      "points": 0,
      "dealer": 0,
      "dead": 0
    }
  },
  "last_toep_player": 0,
  "player_cards": {
    "Martijn": {
      "play_for_points": 1,
      "wait_for_toep_response": 0,
      "pas": 0,
      "wants_to_change_cards": 0,
      "play_with_this_hand": 1
    },
    "Wouter": {
      "cards": [
        {
          "value": "8",
          "suit": "C",
          "played": 0
        },
        {
          "value": "10",
          "suit": "H",
          "played": 0
        },
        {
          "value": "7",
          "suit": "D",
          "played": 0
        },
        {
          "value": "K",
          "suit": "S",
          "played": 0
        }
      ],
      "play_for_points": 1,
      "wait_for_toep_response": 0,
      "pas": 0,
      "wants_to_change_cards": 0,
      "play_with_this_hand": 0
    },
    "Ralph": {
      "play_for_points": 1,
      "wait_for_toep_response": 0,
      "pas": 0,
      "wants_to_change_cards": 0,
      "play_with_this_hand": 1
    }
  },
  "playing_order": [1, 2, 3],
  "round": {
    "Martijn": {
      "played_cards": [],
      "slag": 1
    },
    "Wouter": {
      "played_cards": [],
      "slag": 0
    },
    "Ralph": {
      "played_cards": [],
      "slag": 0
    }
  },
  "round_log": [
    {
      "no": 1,
      "slag": 0,
      "player": 1,
      "action": "nieuwe kaarten"
    },
    {
      "no": 2,
      "slag": 0,
      "player": 2,
      "action": "accepteer"
    },
    {
      "no": 3,
      "slag": 0,
      "player": 3,
      "action": "accepteer"
    },
    {
      "no": 4,
      "slag": 0,
      "player": 2,
      "action": "nieuwe kaarten"
    },
    {
      "no": 5,
      "slag": 0,
      "player": 3,
      "action": "accepteer"
    },
    {
      "no": 6,
      "slag": 0,
      "player": 1,
      "action": "accepteer"
    },
    {
      "no": 7,
      "slag": 0,
      "player": 3,
      "action": "speel"
    },
    {
      "no": 8,
      "slag": 0,
      "player": 1,
      "action": "speel"
    },
    {
      "no": 9,
      "slag": 0,
      "player": 2,
      "action": "nieuwe kaarten"
    },
    {
      "no": 10,
      "slag": 0,
      "player": 3,
      "action": "accepteer"
    },
    {
      "no": 11,
      "slag": 0,
      "player": 1,
      "action": "accepteer"
    }
  ],
  "slag_nr": 0,
  "set_nr": 1
}
```

