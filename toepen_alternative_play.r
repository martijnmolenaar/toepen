

play_action <- function(game, player_i, choice, subjectPlayer = NA){
  round_nr_local <- giveRoundNr(game)

  game_i <- game
  
  ######
  if (choice %in% c("accepteer", "controleer")) {
    ## accepteer of controlleer vieze was
    if (choice == "controleer") {
      ## player wants to check
      cat(">>> ", game_i$player_names[player_i], ": controleer\n", sep =  "")
      game_i <-  checkViezeWasOfPlayer(game_i, askPlayer = player_i, subjectPlayer = subjectPlayer)  ## bij geen vieze was worden nieuwe kaarten gegegeven
    } else {
      ## accepteer
      cat(">>> ", game_i$player_names[player_i], ": accepteer\n", sep =  "")
      ## contolerende speler gelooft het wel
    }
    
    game_i$round_log <-        ## update log
      rbind(game_i$round_log,
            data.frame(
              no = if(identical(game_i$round_log$no, numeric(0))){1} else {last(game_i$round_log$no) + 1},
              slag = round_nr_local,
              player = player_i,
              action = choice
            ))
    
    Sys.sleep(time_delay)   ## wait for half a second
    return(game_i)
  }
  #####
  
  if(choice %in% c("nieuwe kaarten","speel")){   ## vieze was choice
    answer <- ifelse(choice == "nieuwe kaarten", {
        cat(">>> ",game_i$player_names[player_i],": ", "nieuwe kaarten","\n",sep = "")
      
        TRUE
      }, {
        cat(">>> ",game_i$player_names[player_i],": ", "speel","\n",sep = "")
        FALSE
      })
    
    game_i$player_cards[[player_i]]$wants_to_change_cards <-  as.integer(answer)
    
    game_i$round_log <-        ## update log
      rbind(game_i$round_log,
            data.frame(
              no = if(identical(game_i$round_log$no, numeric(0))){1} else {last(game_i$round_log$no) + 1},
              slag = round_nr_local,
              player = player_i,
              action = choice
            ))
    
    Sys.sleep(time_delay)   ## wait for half a second
    return(game_i)
  }  ## end vieze was choice
  
  if (game_i$player_cards[[player_i]]$pas == 0) {
    ## nog in de game?
    
    if (game_i$player_cards[[player_i]]$wait_for_toep_response == 1) {
      ### if somebody toep-ed
      if (choice == "mee"){    
        ### mee
        game_i$player_cards[[player_i]]$wait_for_toep_response <- 0
        game_i$player_cards[[player_i]]$play_for_points <-
          game_i$player_cards[[player_i]]$play_for_points + 1
        
        cat(">>> ",game_i$player_names[player_i],": ", "mee","\n",sep = "")
        
      } else {
        ### pas
        game_i$player_cards[[player_i]]$wait_for_toep_response <- 0
        game_i$player_cards[[player_i]]$pas <- 1
        
        cat(">>> ",game_i$player_names[player_i],": ", "pas","\n",sep = "")
      }
    } else {
      if (choice == "toep") {   
        ### een toep!
        
        # set player to last toep
        game_i$last_toep_player <- player_i
        
        # raise points to win
        game_i$player_cards[[player_i]]$play_for_points <-
          game_i$player_cards[[player_i]]$play_for_points + 1
        
        # set status toep for this player >> ask other active (!!) players to commit
        for (player_j in activePlayers(game_i)[activePlayers(game_i) != player_i]) {
          game_i$player_cards[[player_j]]$wait_for_toep_response <- 1
        }
        
        cat(">>> ",game_i$player_names[player_i],": ", "toep!","\n",sep = "")
      } else {
        ## aan slag ##
        
       
        play_card_nr <-
        which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
          paste(cards$suit, cards$value, sep= "")
        }) == choice)
        
        ## choice == card X
        
        if (game_i$round[[player_i]]$slag[length(game_i$round[[player_i]]$slag)] == 1) {    ## aan slag
          
          if(sum(1-sapply(1:game_i$players, function(i){game_i$player_cards[[i]]$pas}))==1){    ## only one left in the game >> player blank
            
            if(length(play_card_nr) == 0){    ## probably BLANK is played, just take a random card, it doesnt matter
              play_card_nr <- resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0
              })), 1)
            }
              
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <-  round_nr_local
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit <- ""
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value <- factor("BLANK",levels(game_i$values))
            
            game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) +
                                                    1] <-
              list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])
            
          } else {   ## more players are still playing
            
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <- round_nr_local
            
            game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) + 1] <-
              list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])   ## play_card_nr
          }
          
          
          cat(">>> ",game_i$player_names[player_i],": ", 
              game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit,
              as.character(game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value),
              "\n",sep = "")
          
          
          
        } else {
          ## niet aan slag
          
          play_card_nr <-
            which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
              paste(cards$suit, cards$value, sep= "")
            }) == choice)
          
          if(sum(1-sapply(1:game_i$players, function(i){game_i$player_cards[[i]]$pas}))==1){    ## only one left in the game >> player blank
            
            if(length(play_card_nr) == 0){    ## probably BLANK is played, just take a random card, it doesnt matter
              play_card_nr <- resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0
              })), 1)
            }
            
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <-  round_nr_local
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit <- ""
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value <- factor("BLANK",levels(game_i$values))
            
            game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) +
                                                    1] <-
              list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])
            
          } else {   ## take a random card that's not played yet
            ##
            play_card_nr <-
              which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                paste(cards$suit, cards$value, sep= "")
              }) == choice)
            
            
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <-
              round_nr_local
            
            game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) +
                                                    1] <-
              list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])
            
          }
          
          cat(">>> ",game_i$player_names[player_i], ": ", 
              game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit,
              as.character(game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value),
              "\n",sep = "")
          
        }
        
      } ## end else from 'toep' if
      
    } ## end else from respond to toep
    
  } ## end if 'nog in de game?'
  
  game_i$round_log <-        ## update log
    rbind(game_i$round_log,
          data.frame(
            no = if(identical(game_i$round_log$no, numeric(0))){1} else {last(game_i$round_log$no) + 1},
            slag = round_nr_local,
            player = player_i,
            action = choice
          ))
  
  Sys.sleep(time_delay)   ## wait for half a second
  return(game_i)
}


