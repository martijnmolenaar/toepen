options(stringsAsFactors = FALSE)

resample <- function(x, ...) x[sample.int(length(x), ...)]

last <- function(x){       ## return last element from object
  n <- length(x)
  return(x[n])
}


endRound <- function(game, playerWins){
  game_i <- game
  
  game_i$playing_order <- playerWins:(playerWins+game_i$players-1)   ## reset playing order
  while(any(game_i$playing_order > game_i$players)){    ### between 1 and nr_of_players
    game_i$playing_order[game_i$playing_order > game_i$players] <- game_i$playing_order[game_i$playing_order > game_i$players] - game_i$players
  }
  
  
  for(player in 1:game_i$players){
    if(player == playerWins){
      game_i$round[[player]]$slag <- c(game_i$round[[player]]$slag,1)
    } else {
      game_i$round[[player]]$slag <- c(game_i$round[[player]]$slag,0)
    }
  }
  
  Sys.sleep(time_delay)   ## wait for half a second
  return(game_i)
}


pause <- function()
{
  if (interactive())
  {
    invisible(readline(prompt = "Press <Enter> to continue..."))
  }
  else
  {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
  }
}


endSet <- function(game){
  game_i <- game
  
  #if (sum(sapply(1:game_i$players, function(i){ 1 - game_i$player_cards[[i]]$pas })) ==1){    ## nog maar 1 speler in de game? die wint
  if (length(activePlayers(game)) ==1){    ## nog maar 1 speler in de game? die wint
    playerWinsRound <- activePlayers(game)[sapply(activePlayers(game), function(i){ game_i$player_cards[[i]]$pas }) == 0]
  } else {    ## normal end
    evaluation_df <-
      data.frame(player = activePlayers(game, pas = "in"),
                 pas = sapply(activePlayers(game, pas = "in"), function(player){
                   game$player_cards[[player]]$pas
                 }),
                 suit = "",
                 value = 0,
                 slag = NA,
                 suit_to_match = game$round[[which(sapply(game$round, function(x){  
                   x$slag[length(x$slag)]   ==1         }))]]$played_cards[[length(game$round[[which(sapply(game$round, function(x) {
                     x$slag[length(x$slag)]   == 1
                   }))]]$played_cards)]]$suit
      )
    
    evaluation_df$suit[evaluation_df$pas == 0] <-
      sapply(evaluation_df$player[evaluation_df$pas == 0], function(player) {
        n <- length(game$round[[player]]$played_cards)
        game$round[[player]]$played_cards[[n]]$suit
      })
    
    evaluation_df$value[evaluation_df$pas == 0] <-
      sapply(evaluation_df$player[evaluation_df$pas == 0], function(player) {
        n <- length(game$round[[player]]$played_cards)
        game$round[[player]]$played_cards[[n]]$value
      })
    evaluation_df$slag[evaluation_df$pas == 0] <-
      sapply(evaluation_df$player[evaluation_df$pas == 0], function(player) {
        game$round[[player]]$slag[length(game$round[[player]]$slag)]
        
      })
    
    
    evaluation_df$rank <- NA
    ### wie kan matchen en heeft hogere value?
    evaluation_df$rank[evaluation_df$suit_to_match == evaluation_df$suit] <- order(evaluation_df$value[evaluation_df$suit_to_match == evaluation_df$suit],  decreasing = TRUE)
    evaluation_df$rank <- evaluation_df$rank - min(evaluation_df$rank, na.rm = TRUE) + 1
    
    ### this player wins round: slag should be zeroed and set to 1 for this player
    
    playerWinsRound <- evaluation_df$player[which(evaluation_df$rank==1)]
  }
  
  for(i in 1:game_i$players){ 
    if(playerWinsRound != i){  ## losers
      game_i$player_points[[i]]$points <- sum(game_i$player_points[[i]]$points, game_i$player_cards[[i]]$play_for_points)
      game_i$player_cards[[i]]$play_for_points <- 0
      game_i$player_points[[i]]$dealer <- 0
    } else {   ## winners
      game_i$player_cards[[i]]$play_for_points <- 0
      game_i$player_points[[i]]$dealer <- 1
    }
  }
  
  Sys.sleep(time_delay)   ## wait for half a second
  return(game_i)
}

activePlayers <- function(game, pas = "out"){      ## which players are still in the game?
  if(pas == "out"){
    return(  (1:game$players)[
      sapply(1:game$players, function(k){
        game$player_cards[[k]]$pas==0 & game$player_points[[k]]$dead == 0 
      })])
  } 
  if(pas == "in"){
    return(  (1:game$players)[
      sapply(1:game$players, function(k){
        game$player_points[[k]]$dead == 0 
      })])
  }

  
}


initializeGame <- function(numberOfPlayers = 3, player_names = FALSE, gamePlay = "random"){
  values <- factor(c("J", "Q", "K", "A", 7, 8, 9, 10), levels = c("","J", "Q", "K", "A", 7, 8, 9, 10, "BLANK"))
  suits <- c("D", "C", "H", "S")
  
  game_i <- list()
  
  game_i$values <- values
  game_i$suits <- suits
  
  game_i$aflegstapel <- list()
  
  game_i$deck <-
    do.call("c", lapply(values, function(v) {
      lapply(suits, function(s) {
        list(value = v,
             suit = s,
             played = 0)
      })
    }))
  
  game_i$players <- numberOfPlayers
  
  game_i$gamePlay <- data.frame(player = 1:game_i$players,
                     gamePlay = gamePlay)
  ###  max player is 8!!
  
  if(player_names[1] == FALSE | length(player_names) != numberOfPlayers){
    game_i$player_names <- paste("player", 1:game_i$players, sep="")
  } else {
    game_i$player_names <- player_names
  }
  
  
  game_i$player_points <-   sapply(1:game_i$players, function(player_i) {
    list(points = 0,
         dealer = ifelse(player_i==1,1,0),
         dead = 0)
  }, simplify = FALSE)
  
  names(game_i$player_points) <- game_i$player_names
  
  game_i$last_toep_player <- 0
  
  #### game
  
  game_i$deck <- sample(game_i$deck)    ## schudden
  
  
  game_i$player_cards <-     ## verdelen
    sapply(1:game_i$players, function(player_i) {
      list(cards =  game_i$deck[seq(1, 32, 4)[player_i]:seq(4, 32, 4)[player_i]],
           play_for_points = 1,
           wait_for_toep_response = 0,
           pas = 0,
           wants_to_change_cards = 0,
           play_with_this_hand = 0)
    }, simplify = FALSE)
  
  
  game_i$deck <- game_i$deck[        ## cards left in deck
    !sapply(game_i$deck, function(card) {
      paste(card$suit, card$value, sep = "")
    }) %in% unlist(sapply(1:game_i$players, function(player_i) {
      sapply(game_i$player_cards[[player_i]]$cards, function(card) {
        paste(card$suit, card$value, sep = "")
      })
    }, simplify = FALSE))]
  
  
  
  names(game_i$player_cards) <- game_i$player_names
  
  game_i$playing_order <- givePlayerOrder(game_i)
  
  game_i$round <-
    lapply(1:game_i$players, function(player_i){
      list(played_cards = list(),
           slag = ifelse(player_i == game_i$playing_order[1], 1,0))
    })
  names(game_i$round) <- game_i$player_names
  
  game_i$round_log <- data.frame(no = numeric(0),
                                 slag = numeric(0),
                                 player = numeric(0),
                                 action = character(0))
  game_i$slag_nr <- 0
  game_i$set_nr <- 1
  
  return(game_i)
  
  
}

givePlayerOrder <- function(game, startPlayer = "dealer", excludePlayer = 0, exludeDeadPlayers = TRUE, exludePasPlayers = FALSE){
  game_i <- game
  
  if(startPlayer == "dealer"){
    player_order <- c(1:game_i$players,1:game_i$players)
    start_player <- which(sapply(game_i$player_points, function(player_i){  player_i$dealer == 1}))
    player_order <- player_order[(which(player_order == start_player)[1]):  (which(player_order == start_player)[1] +   game_i$players -1)]
    
  } else {
    player_order <- c(1:game_i$players,1:game_i$players)
    start_player <- startPlayer
    player_order <- player_order[(which(player_order == start_player)[1]):  (which(player_order == start_player)[1] +   game_i$players -1)]
  }
  
  if(exludeDeadPlayers){   ## exclude deadPlayers
    deadPlayers <- which(sapply(game_i$player_points, function(player_i){  player_i$dead == 1}))
    player_order <- player_order[!player_order %in% deadPlayers]
  } 
  
  
  if(exludePasPlayers){   ## exclude pasPlayers
    pasPlayers <- which(sapply(game_i$player_cards, function(player_i){  player_i$pas==1}))
    player_order <- player_order[!player_order %in% pasPlayers]
  } 
  
  player_order <- player_order[!player_order %in% excludePlayer]
  
  
  return(player_order)
  
}
  


WhoWinsRound <- function(game){
  
  evaluation_df <-
    data.frame(player = activePlayers(game, pas = "in" ),
               pas = sapply(activePlayers(game, pas = "in" ), function(player){
                 game$player_cards[[player]]$pas
               }),
               suit = "",
               value = 0,
               slag = NA,
               suit_to_match = game$round[[which(sapply(game$round, function(x){  
                 x$slag[length(x$slag)]   ==1         }))]]$played_cards[[length(game$round[[which(sapply(game$round, function(x) {
                   x$slag[length(x$slag)]   == 1
                 }))]]$played_cards)]]$suit
    )
  
  evaluation_df$suit <-
    sapply(evaluation_df$player, function(player) {
      if(length(game$round[[player]]$played_cards) == round_nr){
        game$round[[player]]$played_cards[[round_nr]]$suit
      } else {""}
      
    })
  
  evaluation_df$value<-
    sapply(evaluation_df$player, function(player) {
      if(length(game$round[[player]]$played_cards) == round_nr){
        game$round[[player]]$played_cards[[round_nr]]$value
      } else {factor("", levels = c("","J", "Q", "K", "A", 7, 8, 9, 10, "BLANK"))}
      
    })
  
  evaluation_df$slag<-
    sapply(evaluation_df$player, function(player) {
      game$round[[player]]$slag[length(game$round[[player]]$slag)]
      
    })
  
  evaluation_df$rank <- NA
  ### wie kan matchen en heeft hogere value?
  
  evaluation_df$rank[evaluation_df$suit_to_match == evaluation_df$suit]  <- rank(-as.numeric(evaluation_df$value[evaluation_df$suit_to_match == evaluation_df$suit]))
  evaluation_df$rank <- evaluation_df$rank - min(evaluation_df$rank, na.rm = TRUE) + 1
  
  return(evaluation_df$player[which(evaluation_df$rank==1)])
}



hasViezeWas <- function(game, player){
  
  game_i <- game
  
  no_of_JQKA <-
    sum(sapply(game_i$player_cards[[player]]$cards, function(deck) {
      deck$value %in% factor(c("J", "Q", "K", "A"), levels(game_i$values))
    }))
  
  no_or_sevens <-
    sum(sapply(game_i$player_cards[[player]]$cards, function(deck) {
      deck$value %in% factor(c(7), levels(game_i$values))
    }))
  
  return(no_of_JQKA == 4 | (no_of_JQKA == 3 & no_or_sevens == 1))
  
}


checkViezeWasOfPlayer <- function(game, askPlayer, subjectPlayer) {
  ### speler y gaat speler x controleren op vieze was!
  game_i <- game
  
  if (hasViezeWas(game_i, subjectPlayer)) {
    ## hij heeft inderdaad een vieze was
    hand <- showHandOfPlayer(game_i, subjectPlayer)
    cat("-- vieze was van ", 
        game_i$player_names[subjectPlayer],
        #subjectPlayer,
        ": ",
        paste(hand, collapse = ", "),
        "\n",
        sep = "")
    
    cat("strafpunt voor ", game_i$player_names[askPlayer], "\n", sep = "")
    game_i$player_points[[askPlayer]]$points <-
      game_i$player_points[[askPlayer]]$points + 1
    
    ### pak nieuwe kaarten van deck
    
    game_i <- givePlayerNewCards(game = game_i, player = subjectPlayer)
    
  } else {
    ## hij bluft en heeft geen vieze was
    hand <- showHandOfPlayer(game_i, subjectPlayer)
    cat(
      "-- geen vieze was van ",
      game_i$player_names[subjectPlayer],
      ": ",
      paste(hand, collapse = ", "),
      "\n",
      sep = ""
    )
    
    cat("strafpunt voor ", game_i$player_names[subjectPlayer], "\n", sep = "")
    game_i$player_points[[subjectPlayer]]$points <-
      game_i$player_points[[subjectPlayer]]$points + 1
    
    game_i$player_cards[[subjectPlayer]]$wants_to_change_cards <- 0
    
    ## speel met deze kaarten
    
  }
  Sys.sleep(time_delay)   ## wait for half a second
  return(game_i)
}

showHandOfPlayer <- function(game_i, player){
  stillInHand <- sapply(game$player_cards[[player]]$cards, function(card){
    card$played==0
  })
  
  Hand <- sapply(game$player_cards[[player]]$cards, function(card){
    paste(card$suit, card$value, sep="")
  })[stillInHand]
  
  return(Hand)
  
}



provideChoices <- function(game, player_i){
  choices <- character(0)
  
  if (any(sapply(1:game$players, function(i) {
    game$player_cards[[i]]$wants_to_change_cards == 1
  }))) {
    ### iemand geeft aan een vieze was te hebben, keuze is controleren of geloven
    choices <- c(choices, "accepteer", "controleer")
  } else {
    ## geen vieze was controle
    
    if (game$player_cards[[player_i]]$play_with_this_hand == 0) {
      ### nog in vieze was fase
      choices <- c(choices, "nieuwe kaarten", "speel")
    } else {
      ### het spel zelf
      
      if (game$player_cards[[player_i]]$pas == 0) {
        ## nog in de game?
        
        if (game$player_cards[[player_i]]$wait_for_toep_response == 1) {
          ### if somebody toep-ed
          choices <- c(choices, "pas", "mee")
        } else {
          ### niemand heeft getoept
          if (game$last_toep_player != player_i) {
            ## er kan worden getoept
            choices <- c(choices, "toep")
          }
          
          
          if (game$round[[player_i]]$slag[length(game$round[[player_i]]$slag)] == 1) {
            ## aan slag ##
            if (sum(1 - sapply(1:game$players, function(i) {
              game$player_cards[[i]]$pas
            })) == 1) {
              ## only one left in the game >> player can play blank
              
              choices <- c(choices, "BLANK")
              
              choices <- c(choices,
                           ## nog te spelen kaarten
                           sapply(game$player_cards[[player_i]]$cards, function(cards) {
                             paste(cards$suit, cards$value, sep = "")
                           })[sapply(game$player_cards[[player_i]]$cards, function(cards) {
                             cards$played == 0
                           })])
              
              
            } else {
              ## nog meerdere spelers doen mee
              
              choices <- c(choices,
                           ## nog te spelen kaarten
                           sapply(game$player_cards[[player_i]]$cards, function(cards) {
                             paste(cards$suit, cards$value, sep = "")
                           })[sapply(game$player_cards[[player_i]]$cards, function(cards) {
                             cards$played == 0
                           })])
              
            }
            
            
          } else {
            ## niet aan slag
            
            if (sum(1 - sapply(1:game$players, function(i) {
              game$player_cards[[i]]$pas
            })) == 1) {
              ## only one left in the game >> player can show blank
              choices <- c(choices, "BLANK")
              
              cards_by_slag_player <-
                game$round[[which(sapply(game$round, function(i) {
                  i$slag[length(i$slag)] == 1
                }))]]$played_cards
              
              current_suit_by_slag_player <-
                cards_by_slag_player[[length(cards_by_slag_player)]]$suit
              
              if (any(sapply(game$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0 & cards$suit == current_suit_by_slag_player
              }))) {
                choices <- c(choices,
                             ## nog te spelen kaarten, suit volgen
                             sapply(game$player_cards[[player_i]]$cards, function(cards) {
                               paste(cards$suit, cards$value, sep = "")
                             })[sapply(game$player_cards[[player_i]]$cards, function(cards) {
                               cards$played == 0 & cards$suit == current_suit_by_slag_player
                             })])
                
                
              } else {
                choices <-
                  c(choices,
                    ## nog te spelen kaarten, suit niet volgen, want niet hebben
                    sapply(game$player_cards[[player_i]]$cards, function(cards) {
                      paste(cards$suit, cards$value, sep = "")
                    })[sapply(game$player_cards[[player_i]]$cards, function(cards) {
                      cards$played == 0
                    })])
              }
              
              
              
            } else {
              ## more players in the game, geen slag
              
              cards_by_slag_player <-
                game$round[[which(sapply(game$round, function(i) {
                  i$slag[length(i$slag)] == 1
                }))]]$played_cards
              
              current_suit_by_slag_player <-
                cards_by_slag_player[[length(cards_by_slag_player)]]$suit
              
              if (any(sapply(game$player_cards[[player_i]]$cards, function(cards) {
                cards$played == 0 & cards$suit == current_suit_by_slag_player
              }))) {
                choices <- c(choices,
                             ## nog te spelen kaarten, suit volgen
                             sapply(game$player_cards[[player_i]]$cards, function(cards) {
                               paste(cards$suit, cards$value, sep = "")
                             })[sapply(game$player_cards[[player_i]]$cards, function(cards) {
                               cards$played == 0 & cards$suit == current_suit_by_slag_player
                             })])
              } else {
                choices <-
                  c(choices,
                    ## nog te spelen kaarten, suit niet volgen, want niet hebben
                    sapply(game$player_cards[[player_i]]$cards, function(cards) {
                      paste(cards$suit, cards$value, sep = "")
                    })[sapply(game$player_cards[[player_i]]$cards, function(cards) {
                      cards$played == 0
                    })])
              }
              
              
              
            }
            
          } ## end else from 'toep' if
          
        } ## end else from respond to toep
        
      } ## end if 'nog in de game?'
      
    }
  }
  return(choices)
}


formatChoices <- function(choices, open = TRUE){
  if(open == FALSE){
    choices[choices %in% 
              sapply(c("D", "C", "H", "S"), function(suit) {
                sapply(c("J", "Q", "K", "A", 7, 8, 9, 10), function(value) {
                  paste(suit, value, sep = "")
                })
              })] <- "??"
    
  }
  
  output <- paste(
    sapply(1:length(choices), function(i){
      paste(paste("[",1:length(choices),"]",sep= "")[i],
            choices[i])
      
    }), collapse = "\n")
  

  return(paste(output,"\n"))
  
}




givePlayerNewCards <- function(game, player){
  game_i <- game
  game_i$player_names[player]
  cat("\nnieuwe kaarten voor ",game_i$player_names[player],"\n",sep="")
  
  ## still 4 cards left?
  if (length(game_i$deck) < 4) {
    cat("aflegstapel schudden en naar nieuw deck...\n")
    game_i$deck <- c(game_i$deck, game_i$aflegstapel)
    game_i$aflegstapel <-
      list()                                                           ## leegmaken
    game_i$deck <-
      game_i$deck[resample(1:length(game_i$deck))]                                           ## deck schudden
  }
  
  new_cards <-
    resample(1:length(game_i$deck), 4)                                           ## nrs nieuwe kaarten
  
  game_i$aflegstapel <-
    c(game_i$aflegstapel, game_i$player_cards[[player]]$cards)  ## oude kaarten naar aflegstapel
  game_i$player_cards[[player]]$cards <-
    game_i$deck[new_cards]                     ## nieuwe kaarten van deck naar speler
  game_i$deck <-
    game_i$deck[-new_cards]                                                   ## die kaarten uit deck
  
  game_i$player_cards[[player]]$wants_to_change_cards <- 0
  
  return(game_i)
  
}


setRoundNr <- function(game, round_nr){
  game_i <- game
  game_i$slag_nr <- round_nr
  return(game_i)
}

giveRoundNr <- function(game){
  return(game$slag_nr)
}


chooseRandom <- function(game, player, choices){
  LUT <- data.frame(choices = c("toep","pas","mee","other","speel","nieuwe kaarten", "accepteer","controleer"),
                    rel_prob = c(.2,.3,.7,.4,.5,.5,.9,.1))
  
  ###
  if(all(choices %in% c("speel","nieuwe kaarten"))){
    if (hasViezeWas(game, player)) {
      LUT$rel_prob[LUT$choices == "speel"] <- .1
      LUT$rel_prob[LUT$choices == "nieuwe kaarten"] <- .9
    } else {
      LUT$rel_prob[LUT$choices == "speel"] <- .9
      LUT$rel_prob[LUT$choices == "nieuwe kaarten"] <- .1
      
    }
  }
  
  prob <-
    sapply(choices, function(choice){
      i <- which(choice == LUT$choices)
      if(length(i)==0){
        i <- which(LUT$choices == "other")
      }
      LUT$rel_prob[i]
    })
  prob <- prob / sum(prob)
  return(names(which(runif(1) <   cumsum(prob))[1]))
}


chooseByCommandLine <- function(game, player, choices){
  cat("select choice: ")
  choice <- "0"
  choices_nrs <- as.character(1:length(choices))
  
  while(!(choice %in% choices_nrs)){
    if(interactive()){
      #cat("interactive\n")
      choice <- readline(prompt="")
    } else {
      input <- file("stdin") 
      choice <- readLines(input,1)
      close.connection(input)
    }
    
    if(!(choice %in% choices_nrs)){
      cat("selection not found\n")
      cat("select choice: ")
    }
  }
  
  choice <- choices[as.integer(choice)]
  return(choice)
}

write_game_to_JSON <- function(filename, game){
  con <- file(filename)
  writeLines(text = toJSON(game, pretty = TRUE, auto_unbox = TRUE), con = con)
  close.connection(con = con)
}


managePlayerChoices <- function(game, player, choices){
  
  gamePlay <- game$gamePlay$gamePlay[game$gamePlay$player == player]
  
  if(gamePlay == "manual"){   ## manual player
    
    ## write info file
    write_game_to_JSON(filename = paste("player",player,".txt",sep=""),
                       game = givePlayerEssentialInformation(game, player = player, choices = provideChoices(game, player))
                        )
  
      cat("hand:", showHandOfPlayer(game, player), "\n")
    cat(formatChoices(choices = provideChoices(game, player)))
    choice <- chooseByCommandLine(game, player, choices)
  } 
  
  if(gamePlay == "random"){             ## computer
    cat(formatChoices(choices = provideChoices(game, player), open = FALSE))
    choice <- chooseRandom(game, player, choices)
    
  }
  if(!(gamePlay %in% c("random","manual"))){               ## regard as a shell command
    ## write info file
    write_game_to_JSON(filename = paste("player",player,".txt",sep=""),
                       game = givePlayerEssentialInformation(game, player = player, choices = provideChoices(game, player))
    )
    cat(formatChoices(choices = provideChoices(game, player), open = FALSE))
    
    cat("call '",gamePlay,"'\n", sep="")
    system2(command = unlist(strsplit(gamePlay, " "))[1], 
            args = paste(unlist(strsplit(gamePlay, " "))[-1], collapse = " "))
    
    choice <- fromJSON(txt = paste("player",player,".txt",sep=""))$choices
    
  }
  
  
  return(choice)
}




givePlayerEssentialInformation <- function(game, player, choices){
  gameInfoForPlayer <- c(choices = "",
                         game[names(game) %in% c(
                           'values',
                           'suits',
                           'players',
                           "player_names",
                           "player_points",
                           "last_toep_player",
                           'playing_order',
                           'player_cards',
                           'round',
                           'round_log',
                           'slag_nr',
                           'set_nr'
                         )])
  
  gameInfoForPlayer$choices <-  choices
  
  gameInfoForPlayer$player_cards <-
    sapply(1:gameInfoForPlayer$players, function(player_i) {
      if (player_i == player) {
        cards <- gameInfoForPlayer$player_cards[[player_i]]
      } else {
        cards <-
          gameInfoForPlayer$player_cards[[player_i]][names(gameInfoForPlayer$player_cards[[player_i]]) %in% c(
            "play_for_points",
            "wait_for_toep_response",
            "pas" ,
            "wants_to_change_cards",
            "play_with_this_hand"
          )]   ## don't give cards of other players
      }
      cards
    })
  
  names(gameInfoForPlayer$player_cards) <-
    gameInfoForPlayer$player_names
  
  return(gameInfoForPlayer)
}

giveNextPlayer <- function(game, currentPlayer){
  givePlayerOrder(game, startPlayer = currentPlayer)[2]
}

