options(stringsAsFactors = FALSE)
suppressMessages(
  library(jsonlite)
  )

args <- commandArgs(trailingOnly=TRUE)
filename <- args[which(args == "-f")+1]

invisible(sapply(1:10, function(i){
  cat(".")
  Sys.sleep(.03)
}))

game <- fromJSON(txt = filename)
game$choices <- sample(game$choices,1)    ## make random choice
cat(":",game$choices,"\n" )

con <- file(filename)
writeLines(text = toJSON(game, pretty = TRUE, auto_unbox = TRUE), con = con)
close.connection(con = con)
