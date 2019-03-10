options(stringsAsFactors = FALSE)

invisible(sapply(c('toepen_functions.R', 'toepen_alternative_play.r'), source))
library(jsonlite)

time_delay <- 0.05  #1.5 ### seconds
max_points <- 15

#########

game <- initializeGame(numberOfPlayers = 3,  player_names = c("Martijn", "Wouter","Ralph"),
                       gamePlay = c("random_open", 
                                    #"manual",
                                    #"Rscript randomAI.r -f player2.txt",
                                    #"Rscript randomAI.r -f player3.txt"))
                                    "random_open", 
                                    "random_open"))


# gamePlay = c("random_open", "random", "manual", {shell command})


cat("\n\nHet edele Toep-spel gaat van start...\n\n")


### vieze was

while(length(activePlayers(game )) > 1){     ### game over when only 1 player is left >> he wins
  
  cat("\n\n ############## set #", game$set_nr," ############## \n\n",sep="")
  

player_i <- givePlayerOrder(game)[1]


while(any(sapply(givePlayerOrder(game), function(i){ game$player_cards[[i]]$play_with_this_hand == 0}))){  ## tot alle spelers akkoord zijn
  if(game$player_cards[[player_i]]$play_with_this_hand == 1){
    player_i <- giveNextPlayer(game, player_i)
    next
    }    ## this player already said he wants to play
  
  cat("\n", game$player_names[player_i],":\n",sep="")

  choice <- managePlayerChoices(game, player = player_i, 
                      choices = provideChoices(game, player_i))

  game <- play_action(game, player_i, choice)   ## just testing
  
    if(game$player_cards[[player_i]]$wants_to_change_cards==1){   ## player wants to change

    ## check other players
    
    for(playerToCheck_i in givePlayerOrder(game, startPlayer = player_i, excludePlayer = player_i)){

      cat("\n", game$player_names[playerToCheck_i],":\n",sep="")
     
      choice <- managePlayerChoices(game, player = playerToCheck_i, 
                                    choices = provideChoices(game, playerToCheck_i))
      
      game <- play_action(game, player_i = playerToCheck_i, choice = choice, subjectPlayer = player_i)
      if (choice == "controleer") { break }    ## break for loop: speler moet of doorspeler met oude kaarten, of heeft nieuwe kaarten gekregen
      
    } ## end for
    
    if(choice !=  "controleer"){    game <- givePlayerNewCards(game, player_i) }
     
    game$player_cards[[player_i]]$wants_to_change_cards <- 0
    
    
  } else {       
    ## player don't want to change, so want to play with this hand
    game$player_cards[[player_i]]$play_with_this_hand <- 1
  } 
  
  player_i <- giveNextPlayer(game, player_i)
}

cat("\n")

####### iterate 4 rounds

for(round_nr in 1:4){
  
  game <- setRoundNr(game, round_nr)
  cat("\n -------------- slag #", game$slag_nr," -------------- \n\n",sep="")
  
  while( !all(sapply(activePlayers(game), function(i){length(game$round[[i]]$played_cards)}) == round_nr) &  ## nog minder kaarten gespeeld door actieve sperers dan het rondenr
        length(activePlayers(game))  > 1){            ## en meer dan 1 speler in de game
    
    game$playing_order <- givePlayerOrder(game, exludePasPlayers = TRUE, startPlayer = "pas_over_dealer")

    for (player_i in game$playing_order) {   ## play round
      
      if(game$player_cards[[player_i]]$pas == 1){ 
        next}  ##  escape this iteration of for loop when player already passed
      
      cat("\n", game$player_names[player_i],ifelse((game$round[[player_i]]$slag==1)[length(game$round[[player_i]]$slag)],
                                                          " (aan slag)",""),":\n",sep="")
      
      choice <- managePlayerChoices(game, player = player_i, 
                          choices = provideChoices(game, player_i))
      
      # if(player_i == 1){browser()}
      
      game <- play_action(game, player_i, choice)   ## play action
      

      if(max(sapply(activePlayers(game), function(i){game$player_cards[[i]]$wait_for_toep_response}))==1){   ## waiting for toep response?
        check_order <- givePlayerOrder(game, startPlayer = giveNextPlayer(game, player_i),exludePasPlayers = TRUE)

        for(player_i_check in check_order){
          
          cat("\n", game$player_names[player_i_check],
              ifelse((game$round[[player_i_check]]$slag==1)[length(game$round[[player_i_check]]$slag)],
                                                              " (aan slag)",""),":\n",sep="")
          
          
          choice <- managePlayerChoices(game, player = player_i_check, 
                                        choices = provideChoices(game, player_i_check))
          
          game <- play_action(game, player_i_check, choice = choice)   ## just testing
          
        }
      }
      
      
    }
    

  }




### wie won de slag or game?
  
  
#if (sum(sapply(1:game$players, function(i){ 1 - game$player_cards[[i]]$pas })) ==1){    ## nog maar 1 speler in de game? die wint
if (length(activePlayers(game)) ==1){    ## nog maar 1 speler in de game? die wint
  playerWins <- activePlayers(game)[sapply(activePlayers(game), function(i){ 1 - game$player_cards[[i]]$pas ==1})]

  cat("\n",game$player_names[playerWins] ," wint", "\n\n",sep = "")
  game <- endRound(game, playerWins = playerWins)
  break

} else {     ## hele ronde gespeeld
  
  playerWins <- WhoWinsRound(game)
  cat("\n",game$player_names[playerWins]," wins", "\n\n",sep = "")
  
  ### player 1 wint slag, maar is gepast, dan speelt hij nog wel verder denk ik, pas ongedaan (points at stake blijft dus 1 verschillen)
  if(game$player_cards[[playerWins]]$pas == 1){
    cat("speciale situatie: speler wint slag maar is gepast >>> speler speelt weer mee...\n")
    game$player_cards[[playerWins]]$pas <- 0
  }
  
  
  game <- endRound(game, playerWins = playerWins)
  
}
  


} # end for round_nr

game <- endSet(game, limit = max_points)

invisible(sapply(1:game$players, function(i){
  cat(game$player_names[i] ,": ",game$player_points[[i]]$points, " punten",
      ifelse(game$player_points[[i]]$dead," (dood)\n","\n"), sep="")
  }))

Sys.sleep(1)

game <- initializeNewSet(game)

}

cat("\n\n -----> game won by ",
game$player_names[sapply(1:game$players, function(i) {
  game$player_points[[i]]$points
}) ==
  min(sapply(1:game$players, function(i) {
    game$player_points[[i]]$points
  }))],"\n",sep = "")


con <- file("game.txt")
writeLines(text = toJSON(game, pretty = TRUE, auto_unbox = TRUE), con = con)
close.connection(con = con)


