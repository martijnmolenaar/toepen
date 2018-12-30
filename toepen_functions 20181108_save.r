options(stringsAsFactors = FALSE)


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
  
  if (sum(sapply(1:game_i$players, function(i){ 1 - game_i$player_cards[[i]]$pas })) ==1){    ## nog maar 1 speler in de game? die wint
    playerWinsRound <- (1:game_i$players)[sapply(1:game_i$players, function(i){ game_i$player_cards[[i]]$pas }) == 0]
  } else {    ## normal end
    evaluation_df <-
      data.frame(player = 1:game$players,
                 pas = sapply(1:game$players, function(player){
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
  
  return(game_i)
}
