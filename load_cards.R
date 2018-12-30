options(stringsAsFactors = FALSE)
library(imager)

deck_image <- load.image('toepen_deck.png')

plot(deck_image)

dim(deck_image)
between <- function(i,mid,range){i > (mid-range) & i < (mid+range)}


cards_x <- sapply(1:8, function(j){
  i <- seq(0,947,length.out = 9)
  mean(i[c(j,j+1)])
  })

cards_y <- sapply(1:4, function(j){
  i <- seq(0,614,length.out = 5)
  mean(i[c(j,j+1)])
})

plot(axes = FALSE,
     imsub(deck_image,
           between(x, sample(cards_x,1),60),
           between(y, sample(cards_y,1),82)) #First 30 columns and rows
)

values <- factor(c("J", "Q", "K", "A", 7, 8, 9, 10), levels = c("J", "Q", "K", "A", 7, 8, 9, 10))
suits <- c("D", "C", "H", "S")

# diamonds(♦) clubs(♣) hearts(♥) spades(♠) 
card_pictures <- 
sapply(values, function(value){
  sapply(suits, function(suit){
    if(as.numeric(value) < 5){
           imsub(deck_image,
                 between(x, cards_x[which(suits == suit)],60),  
                 between(y, cards_y[which(values == value)],82)) #First 30 columns and rows
    } else {
           imsub(deck_image,
                 between(x, cards_x[which(suits == suit)+4],60),  
                 between(y, cards_y[which(values == value)-4],82)) #First 30 columns and rows
     }
  }, simplify = FALSE)
}, simplify = FALSE)

names(card_pictures) <- values

plot(axes = TRUE, xlim = c(-100,200),
card_pictures[['7']][['S']]
)
