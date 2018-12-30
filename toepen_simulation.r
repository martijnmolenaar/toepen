options(stringsAsFactors = FALSE)

source('toepen_functions_for_simulation.R')
library(jsonlite)

time_delay <- 0  #5  #1.5 ### seconds

#########
### to initiate
InitiateSimulationGame <- function(gameProgress, currentPlayer){
  game_i <- gameProgress
  game_i$aflegstapel <- list()
  
  game_i$player_names <- unlist(game_i$player_names)   ##  vector format
  game_i$playing_order <- unlist(game_i$playing_order) ##  vector format
  for(player_j in game_i$playing_order){               ##  vector format
    game_i$round[[player_j]]$slag <- unlist(game_i$round[[player_j]]$slag)
  }
  
  game_i$deck <-
    do.call("c", lapply(game_i$values, function(v) {
      lapply(game_i$suits, function(s) {
        list(value = v,
             suit = s,
             played = 0)
      })
    }))
  
  game_i$round_log <-     ## as.data.frame
  as.data.frame(t(sapply(1:length(game_i$round_log), function(x){
    unlist(game_i$round_log[[x]])
    })))
  
  game_i$round_log$no <- as.numeric(game_i$round_log$no)
  game_i$round_log$slag <- as.numeric(game_i$round_log$slag)
  game_i$round_log$player <- as.numeric(game_i$round_log$player)
  
  #names(game_i$player_cards) <- NULL
  
  #### game
  
  game_i$deck <- sample(game_i$deck)    ## schudden
  
  ## as list
  #game_i$player_cards[[currentPlayer]]$cards <-   apply(game_i$player_cards[[currentPlayer]]$cards, 1,as.list)
  
  ## cards left in deck (used by player)
  game_i$deck <-
    game_i$deck[   
      !sapply(game_i$deck, function(card) {
        paste(card$suit, card$value, sep = "")
      }) %in%
        sapply(game_i$player_cards[[currentPlayer]]$cards, function(card) {
          paste(card$suit, card$value, sep = "")
        })]
  
  
  cards_played <-
    game_i$round_log[game_i$round_log$action %in% sapply(game_i$deck, function(card) {
      paste(card$suit, card$value, sep = "")
    }), ]
  
  cards_played <-  cards_played[cards_played$player != currentPlayer,]
  
  
  for (row in 1:dim(cards_played)[1]) {
    ## browse through cards in game
    
    temp_card <- game_i$deck[sapply(game_i$deck, function(card) {
      paste(card$suit, card$value, sep = "")
    })  %in% cards_played[row, ]$action]
    
    temp_card[[1]]$played <-  cards_played[row, ]$slag
    
    game_i$player_cards[[as.numeric(cards_played[row, ]$player)]]$cards <- c(game_i$player_cards[[as.numeric(cards_played[row, ]$player)]]$cards,temp_card    )
    
    game_i$deck <- game_i$deck[!sapply(game_i$deck, function(card) {   ## remove from deck
      paste(card$suit, card$value, sep = "")
    })  %in% cards_played[row, ]$action]
    
    
  }
  
  
  player_cards <-     ## haal gespeelde kaarten uit het deck naar hands
    sapply(1:game_i$players, function(player_i) {
      cards_missing <- 4 - length(game_i$player_cards[[player_i]]$cards)
      if(cards_missing > 0){
        c(game_i$player_cards[[player_i]]$cards,
          game_i$deck[(seq(1, 32, 4)[player_i]:seq(4, 32, 4)[player_i])[1:cards_missing]   ])
      } else {
        game_i$player_cards[[player_i]]$cards
      }
      
    }, simplify = FALSE)
  
  #for(player_i in 1:game_i$players){
  #  [[player_i]]$cards <- player_cards[[player_i]]
  #}
  
  game_i$deck <- game_i$deck[        ## cards left in deck
    !sapply(game_i$deck, function(card) {
      paste(card$suit, card$value, sep = "")
    }) %in% unlist(sapply(1:game_i$players, function(player_i) {
      sapply(game_i$player_cards[[player_i]]$cards, function(card) {
        paste(card$suit, card$value, sep = "")
      })
    }, simplify = FALSE))]
  
  
  #for(player_i in 1:game_i$players){
  #  if(length(game_i$round$Martijn$played_cards) != 0){
  #    game_i$round[[player_i]]$played_cards <- apply(game_i$round[[player_i]]$played_cards, 1, as.list)
  #  } 
    
  #}
  
  
  
  return(game_i)
  
}


ExecuteSimulationGame <- function(game, action_choice){
  game_i <- game
  
  #game_i <- play_action(game_i, player_i = 1, action_choice)   ## play choice
  game_i$skip <- TRUE
  
  start_slag <- game_i$slag_nr
  
  for(round_nr in start_slag:4){
    
    game_i <- setRoundNr(game_i, round_nr)
    cat("\n -------------- slag #", round_nr," -------------- \n\n",sep="")
    
    while( !all(sapply(activePlayers(game_i), function(i){length(game_i$round[[i]]$played_cards)}) == round_nr) &  ## nog minder kaarten gespeeld door actieve sperers dan het rondenr
           length(activePlayers(game_i))  > 1){            ## en meer dan 1 speler in de game
      
      game_i$playing_order <- givePlayerOrder(game_i, exludePasPlayers = TRUE, startPlayer = "pas_over_dealer")
      
      for (player_i in game_i$playing_order) {   ## play round
        #browser()
        if(player_i %in% game$round_log$player[game$round_log$slag==round_nr]){
          next}  ## if this player already played in log, skip
        
        if(game_i$player_cards[[player_i]]$pas == 1){ 
          next}  ##  escape this iteration of for loop when player already passed
        #browser()
        if(game_i$skip){   ## this action is set in the argument
          cat("\n", game_i$player_names[player_i],ifelse((game_i$round[[player_i]]$slag==1)[length(game_i$round[[player_i]]$slag)],
                                                         " (aan slag)",""),":\n",sep="")
          choice <- action_choice
          game_i <- play_action(game_i, player_i, choice) 
          game_i$skip <- FALSE
        } else {   ## normal action move
          
          cat("\n", game_i$player_names[player_i],ifelse((game_i$round[[player_i]]$slag==1)[length(game_i$round[[player_i]]$slag)],
                                                         " (aan slag)",""),":\n",sep="")
          choice <- chooseRandom(game_i, player, {c <- provideChoices(game_i, player_i); names(c) <- NULL; c})
          
          game_i <- play_action(game_i, player_i, choice)   ## just testing
          
            
          } 
        
        
        
       
      }
      
      
    }
    
    
    ### wie won de slag or game?
    
    #browser()
    if (length(activePlayers(game_i)) ==1){    ## nog maar 1 speler in de game? die wint
      playerWins <- activePlayers(game_i)[sapply(activePlayers(game_i), function(i){ 1 - game_i$player_cards[[i]]$pas ==1})]
      
      cat("\n",game_i$player_names[playerWins] ," wint", "\n\n",sep = "")
      game_i <- endRound(game_i, playerWins = playerWins)
      break
      
    } else {     ## hele ronde gespeeld
      
      playerWins <- WhoWinsRound(game_i)
      cat("\n",game_i$player_names[playerWins]," wins", "\n\n",sep = "")
      
      ### player 1 wint slag, maar is gepast, dan speelt hij nog wel verder denk ik, pas ongedaan (points at stake blijft dus 1 verschillen)
      if(game_i$player_cards[[playerWins]]$pas == 1){
        cat("speciale situatie: speler wint slag maar is gepast >>> speler speelt weer mee...\n")
        game_i$player_cards[[playerWins]]$pas <- 0
      }
      
      
      game_i <- endRound(game_i, playerWins = playerWins)
      
    }
    
    
    
  } # end for round_nr
  return(playerWins)
}


smallGame <- fromJSON(txt = 'player1_example.txt', simplifyVector = FALSE)
#smallGame$round_log
#smallGame <- fromJSON(txt = 'player1_example.txt', simplifyVector = TRUE)

#smallGame <- fromJSON(txt = 'player1_example2.txt', simplifyVector = FALSE)
game <- InitiateSimulationGame(gameProgress = smallGame, currentPlayer = 1)

str(game$round_log)

ExecuteSimulationGame(game, action_choice = game$choices[1])

str(game)

k <- numeric(0)
for(i in 1:100){
  game2 <- InitiateSimulationGame(gameProgress = smallGame, currentPlayer = 1)
  k <- c(k,ExecuteSimulationGame(game2, choice = game2$choices))
}

table(k)
