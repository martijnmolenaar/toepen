options(stringsAsFactors = FALSE)

invisible(sapply(c('toepen_AI.r','toepen_functions.R'), source))

library(jsonlite)


values <- factor(c("J", "Q", "K", "A", 7, 8, 9, 10), levels = c("J", "Q", "K", "A", 7, 8, 9, 10, "BLANK"))
suits <- c("D", "C", "H", "S")

evaluations_for_testing <- list()

game <- list()

game$deck <-
  do.call("c", lapply(values, function(v) {
    lapply(suits, function(s) {
      list(value = v,
           suit = s,
           played = 0)
    })
  }))


game$players <- 3
###  max player is 8!!
game$player_names <- paste("player", 1:game$players, sep="")

game$player_points <-   sapply(1:game$players, function(player_i) {
  list(points = 0,
       dealer = ifelse(player_i==1,1,0))
}, simplify = FALSE)

names(game$player_points) <- game$player_names

game$last_toep_player <- 0

#### game

game$deck <- sample(game$deck)    ## schudden


game$player_cards <-     ## verdelen
  sapply(1:game$players, function(player_i) {
    list(cards =  game$deck[seq(1, 32, 4)[player_i]:seq(4, 32, 4)[player_i]],
         play_for_points = 1,
         wait_for_toep_response = 0,
         pas = 0)
  }, simplify = FALSE)


game$deck <- game$deck[        ## cards left in deck
!sapply(game$deck, function(card) {
  paste(card$suit, card$value, sep = "")
}) %in% unlist(sapply(1:game$players, function(player_i) {
  sapply(game$player_cards[[player_i]]$cards, function(card) {
    paste(card$suit, card$value, sep = "")
  })
}, simplify = FALSE))]



names(game$player_cards) <- game$player_names


game$playing_order <- which(sapply(game$player_points, function(player_i){  player_i$dealer == 1})) : game$players


while(any(game$playing_order > game$players)){    ### between 1 and nr_of_players
  game$playing_order[game$playing_order > game$players] <- game$playing_order[game$playing_order > game$players] - game$players
}

game$round <-
lapply(1:game$players, function(player_i){
  list(played_cards = list(),
       slag = ifelse(player_i == game$playing_order[1], 1,0))
})
names(game$round) <- game$player_names


####### iterate 4 rounds

for(round_nr in 1:4){
  
  while((max(sapply(1:game$players, function(i){length(game$round[[i]]$played_cards)})) < round_nr) &  ## nog minder kaarten gespeeld dan het rondenr
        (sum(sapply(1:game$players, function(i){ 1 - game$player_cards[[i]]$pas }))  > 1)){            ## en meer dan 1 speler in de game
    
    for (player_i in game$playing_order) {   ## play round
      
      game <- play(game, player_i, round_nr = round_nr)
      
      cat("   cards played by player",player_i,": ",sapply(1:4, function(card_i){game$player_cards[[player_i]]$cards[[card_i]]$played}), "\n",sep = "")
      
      
      if(max(sapply(1:game$players, function(i){game$player_cards[[i]]$wait_for_toep_response}))==1){   ## waiting for toep response?
        check_order <- c(game$playing_order,game$playing_order,game$playing_order)
        check_order <- check_order[(which(check_order==player_i)[1]+1):(which(check_order==player_i)[2])]
        for(player_i_check in check_order){
          game <- play(game, player_i_check, round_nr_local = round_nr)
          
          cat("   cards played by player",player_i,": ",sapply(1:4, function(card_i){game$player_cards[[player_i]]$cards[[card_i]]$played}), "\n",sep = "")
          
        }
      }
      
      
    }
    

  }




### wie won de slag or game?
if (sum(sapply(1:game$players, function(i){ 1 - game$player_cards[[i]]$pas })) ==1){    ## nog maar 1 speler in de game? die wint
  playerWins <- (1:game$players)[sapply(1:game$players, function(i){ 1 - game$player_cards[[i]]$pas ==1})]

  cat("round: ",round_nr,"\n","player",playerWins," wins", "\n\n",sep = "")
  game <- endRound(game, playerWins = playerWins)
  break

} else {     ## hele ronde gespeeld
  
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

  evaluations_for_testing <- c(evaluations_for_testing, list(evaluation_df[game$playing_order,]))
  ### this player wins round: slag should be zeroed and set to 1 for this player
  
  playerWins <- evaluation_df$player[which(evaluation_df$rank==1)]
  cat("round: ",round_nr,"\n","player",playerWins," wins", "\n\n",sep = "")
  
  game <- endRound(game, playerWins = playerWins)
}
  


} # end for round_nr

game <- endSet(game)

invisible(sapply(1:game$players, function(i){
  cat("player",i,": ",game$player_points[[i]]$points, " points\n",sep="")
  
  }))




#####
#overview <- 
#as.data.frame(t(sapply(1:game$players, function(player){
#  paste(
#  ifelse(game$round[[player]]$slag[1:4]==1,"^",""),
#  sapply(game$round[[player]]$played_cards, function(card){paste(card$suit, card$value, sep = "")}),
#  ifelse(game$round[[player]]$slag[2:5], "*",""),
#  sep = "")
#})))
#rownames(overview) <- paste("player",1:dim(overview)[1], sep="")
#colnames(overview) <- paste("round",1:dim(overview)[2], sep="")


#View(overview)


#evaluations_for_testing <-
#do.call("rbind",
#lapply(1:length(evaluations_for_testing), function(i){
#  p <- evaluations_for_testing[[i]]
#  p$round <- i
#  p
#}))

#View(evaluations_for_testing)





con <- file("game.txt")
writeLines(text = toJSON(game, pretty = TRUE, auto_unbox = TRUE), con = con)
close.connection(con = con)


