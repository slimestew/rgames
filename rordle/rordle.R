# install.packages("words")
library(words)
guesses <- 1
guess <- ""

fiveLength <- subset(words, word_length == 5)
word <- sample(fiveLength$word, 1)
#"gleek"
#"caulk"

drawGuess <- function(word, guess, guesses){
  res <- rep(".", times = 5) #incorrect
  for(i in 1:5){ #correct letter and place
     if(tolower(substr(guess, i, i)) == substr(word, i, i)){
       res[i] <- "!"
       word <- paste0(substr(word, 1, i - 1), " ", substr(word, i + 1, nchar(word))) #words beyond 5 char heehee
     }
   }
    
  for(i in 1:5){ #exists but not correct place
    if(grepl(substr(guess, i, i), word, ignore.case = TRUE) && res[i]!="!"){
      res[i] <- "?"
      word <- paste0(substr(word, 1, gregexpr(substr(guess, i, i), word, ignore.case = TRUE)[[1]] - 1), " ",
                      substr(word, gregexpr(substr(guess, i, i), word, ignore.case = TRUE)[[1]] + 1, nchar(word)))
    }
  }
  return(c(res,guesses))
}

while(guesses<7 && tolower(guess) != word){
  guess <- readline()
  if(nchar(guess)==5 && tolower(guess) %in% fiveLength$word) {
    drawn <- drawGuess(word, guess, guesses)
    cat(drawn)
    cat("\n")
    guesses <- guesses + 1
  } else {
    cat("That word is invalid\n")
  }
}

if(tolower(guess) == word){
  cat("You Win!")
} else {
  cat(paste("You Lose! The word was",word))
}
