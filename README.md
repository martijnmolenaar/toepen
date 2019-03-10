# Toepen gameplay-simulator in R

### installation and running

Install R via [CRAN](https://cran.r-project.org/)

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

When a player's `GamePlay` is set to a terminal command, `toepen.r` returns a file called 'player1.txt` for player1, etc. 
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

The called program should return the same file, with `choices` set to the proposed gameplay choice. 
An example program (`randomAI.r`) is added.

