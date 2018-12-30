options(stringsAsFactors = FALSE)

resample <- function(x, ...) x[sample.int(length(x), ...)]

play <- function(game, player_i, round_nr_local){
  toep_prob <- .2 #.7
  play_after_toep_prob <- .7  #.7 #.1
  
  game_i <- game
  
  if (game_i$player_cards[[player_i]]$pas == 0) {
    ## nog in de game?
    
    if (game_i$player_cards[[player_i]]$wait_for_toep_response == 1) {
      ### if somebody toep-ed
      if (runif(1) < play_after_toep_prob){    
        ### mee
        game_i$player_cards[[player_i]]$wait_for_toep_response <- 0
        game_i$player_cards[[player_i]]$play_for_points <-
          game_i$player_cards[[player_i]]$play_for_points + 1
        
        cat("round: ",round_nr_local,"\n","player",player_i,": ", "mee","\n",sep = "")
        
      } else {
        ### pas
        game_i$player_cards[[player_i]]$wait_for_toep_response <- 0
        game_i$player_cards[[player_i]]$pas <- 1
        
        cat("round: ",round_nr_local,"\n","player",player_i,": ", "pas","\n",sep = "")
      }
    } else {
      if (runif(1) < toep_prob & game_i$last_toep_player != player_i) {   
        ### een toep!
        
        # set player to last toep
        game_i$last_toep_player <- player_i
        
        # raise points to win
        game_i$player_cards[[player_i]]$play_for_points <-
          game_i$player_cards[[player_i]]$play_for_points + 1
        
        # set status toep for this player >> ask other players to commit
        for (player_j in (1:game_i$players)[1:game_i$players != player_i]) {
          game_i$player_cards[[player_j]]$wait_for_toep_response <- 1
        }
        
        
        
        cat("round: ",round_nr_local,"\n",ifelse((game_i$round[[player_i]]$slag[length(game_i$round[[player_i]]$slag)] == 1),"*",""),
            "player",player_i,": ", "toep!","\n",sep = "")
      } else {
        ## aan slag ##
        if (game_i$round[[player_i]]$slag[length(game_i$round[[player_i]]$slag)] == 1) {
          
          if(sum(1-sapply(1:game_i$players, function(i){game_i$player_cards[[i]]$pas}))==1){    ## only one left in the game >> player blank
            
            play_card_nr <-
              resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0
              })), 1)
            
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <-  round_nr_local
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit <- ""
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value <- factor("BLANK",levels(values))
            
            game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) +
                                                    1] <-
              list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])
        
          } else {   ## take a random card that's not played yet
            play_card_nr <-
              resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0
              })), 1)         #### something goes wrong here sometimes....
            
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <- round_nr_local
            
            game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) + 1] <-
              list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])
          }
          
          
          cat("round: ",round_nr_local,"\n","*player",player_i,": ", game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit,
                                                 as.character(game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value),
                                      "\n",sep = "")
          
          
          
        } else {
          ## niet aan slag
          
          if(sum(1-sapply(1:game_i$players, function(i){game_i$player_cards[[i]]$pas}))==1){    ## only one left in the game >> player blank
            
            cards_by_slag_player <-
              game_i$round[[which(sapply(game_i$round, function(i) {
                i$slag[length(i$slag)] == 1
              }))]]$played_cards
            
            current_suit_by_slag_player <-
              cards_by_slag_player[[length(cards_by_slag_player)]]$suit
            
            if (any(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
              cards$played == 0 & cards$suit == current_suit_by_slag_player
            }))) {
              play_card_nr <-
                resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                  cards$played == 0 & cards$suit == current_suit_by_slag_player
                })), 1)
            } else {
              play_card_nr <-
                resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                  cards$played == 0
                })), 1)
            }
            
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <-  round_nr_local
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit <- ""
            game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value <- factor("BLANK",levels(values))
            
            game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) +
                                                    1] <-
              list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])
            
          } else {   ## take a random card that's not played yet
          ##
          cards_by_slag_player <-
            game_i$round[[which(sapply(game_i$round, function(i) {
              i$slag[length(i$slag)] == 1
            }))]]$played_cards
          
          current_suit_by_slag_player <-
            cards_by_slag_player[[length(cards_by_slag_player)]]$suit
          
          if (any(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
            cards$played == 0 & cards$suit == current_suit_by_slag_player
          }))) {
            play_card_nr <-
              resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0 & cards$suit == current_suit_by_slag_player
              })), 1)
          } else {
            play_card_nr <-
              resample(which(sapply(game_i$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0
              })), 1)
          }
          
          
          game_i$player_cards[[player_i]]$cards[[play_card_nr]]$played <-
            round_nr_local
          
          game_i$round[[player_i]]$played_cards[length(game_i$round[[player_i]]$played_cards) +
                                                  1] <-
            list(game_i$player_cards[[player_i]]$cards[[play_card_nr]])
          
          }
          
          cat("round: ",round_nr_local,"\n","player",player_i,": ", game_i$player_cards[[player_i]]$cards[[play_card_nr]]$suit,
              as.character(game_i$player_cards[[player_i]]$cards[[play_card_nr]]$value),
              "\n",sep = "")
          
        }
        
      } ## end else from 'toep' if
      
    } ## end else from respond to toep
    
  } ## end if 'nog in de game?'
  
  return(game_i)
}

