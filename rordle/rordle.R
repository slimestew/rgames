# install.packages("words")
library(words)
guesses <- 1
guess <- ""

fiveLength <- subset(words, word_length == 5)
word <- sample(fiveLength$word, 1)
letterGuesses <- rep(" ", times = 26)
hardmode <- FALSE
#"gleek"
#"caulk"

drawGuess <- function(word, guess, guesses){ #assumes all lowercase
  res <- rep(".", times = 5) #incorrect
  for(i in 1:5){ #correct letter and place
     if(substr(guess, i, i) == substr(word, i, i)){
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

drawLetters <- function(letterGuesses){
  keyb <- c(17, 23, 5, 18, 20, 25, 21, 9, 16, 10,
            1, 19, 4, 6, 7, 8, 12, 11, 13,
            26, 24, 3, 22, 2, 14, 15)
  for(i in 1:26){
    cat(paste0(intToUtf8(keyb[i] + 64), "[",letterGuesses[keyb[i]],"]"))
    if(i==10 || i==19 || i==26)
      cat("\n")
  }
}

hardmodetest <- function(word, guess, letterGuesses){
  for(i in 1:5){ 
    if(letterGuesses[[utf8ToInt(substr(tolower(guess),i,i)) - 96]] == ".")# '.' only shows if the letter does not show up at all
      return(FALSE) #green and yellow are acceptable
    #do we need bonus checks for confirmed guesses or dupes? maybe add second column to letterGuesses if so
  }
  return(TRUE)
}

cat("R-dle")
while(guesses<7 && tolower(guess) != word){
  guess <- readline()
  
  if(nchar(guess)==5 && tolower(guess) %in% fiveLength$word && ((hardmode && hardmodetest(word, guess, letterGuesses)) || !hardmode) ) {
    drawn <- drawGuess(word, tolower(guess), guesses)
    cat(drawn)
    cat("\n")
    guesses <- guesses + 1
    for(i in 1:5){
      if(letterGuesses[[utf8ToInt(substr(tolower(guess),i,i)) - 96]] != "!" || drawn[i] == "!") #only stores if either better than nothing or correct
        letterGuesses[utf8ToInt(substr(tolower(guess),i,i)) - 96] <- drawn[i]
    }
  } else if(tolower(guess) == "letters") {
    drawLetters(letterGuesses)
  } else if(tolower(guess) == "hardmode") {
    hardmode <- !hardmode
    cat(paste("Hard Mode is now",ifelse(hardmode,"on","off")))
  } else {
    cat("That word is invalid\n")
  }
}

if(tolower(guess) == word){
  cat("You Win!")
} else {
  cat(paste("You Lose! The word was",word))
}
