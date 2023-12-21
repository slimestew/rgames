par(bg = "slategray")
board <- matrix(0, nrow = 6, ncol = 7)
menu <- 0
click <- list(x = 3, y = 3)
options <- c(0, 1, 2, 0)
# Player 1 piece color
# Player 2 piece color
# Board color
# Human or Computer (computer can go first or second)
turn <- 1
fiarColors <- c("red", "yellow", "blue", "green", "orange", "cyan", "purple", "white", "pink", "chartreuse", "tan", "darkorange4")

dropp <- function(col, player, board){
  for(r in 6:1)
    if(board[r,col]==0){
      board[r,col] <- player
      return(board)
    }
  return(NA)
}

checkwin <- function(board){ 
  for(i in 1:6) #horizontal
    if(any(apply(embed(board[i,], 4), 1, function(r) (all(r==r[1]) && r[1]!=0)))) #if all in a row subset are equal and not 0
      return(TRUE)
  
  for(i in 1:7) #vertical
    if(any(apply(embed(board[,i], 3), 2, function(r) (all(r==r[1]) && r[1]!=0)))) #columns
       return(TRUE)
  
  for(i in 1:3) #secondary diagonal
    for(j in 1:4){
      tempcheck <- board[i,j]
      for(k in 1:3){
        if(board[i+k,j+k] != tempcheck || tempcheck == 0)
          break
        if(k==3)
          return(TRUE)
      }
    }
  for(i in 1:3) #primary diagonal
    for(j in 7:4){
      tempcheck <- board[i,j]
      for(k in 1:3){
        if(board[i+k,j-k] != tempcheck || tempcheck == 0)
          break
        if(k==3)
          return(TRUE)
      }
    }
  return(FALSE)
}

doAI <- function(board, human){
  for(i in 1:7){ #offense
    temp <- dropp(i, 3-human, board)
    if(!any(is.na(temp)) && checkwin(temp))
      return(temp)
  }
  for(i in 1:7){ #defense
    temp <- dropp(i, human, board)
    if(!any(is.na(temp)) && checkwin(temp))
      return(dropp(i, 3-human, board))
  }
  # scope should be small, but a double loop that sees both player's options would perform much better
  # and not occasionally hand the player a set up win
  for(i in sample(1:8)){ #random
    if(i<8)
      temp <- dropp(i, 3-human, board)
    else #edge towards center
      temp <- dropp(4, 3-human, board)
    if(!any(is.na(temp)))
      return(temp)
  }
  return(board) #no possible moves
}

plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
lines(c(7.75, 7.75), c(7, 9), lwd = 3, col = "red")
lines(c(7.25, 7.25), c(6.5, 8.5), lwd = 3, col = "red")
symbols(c(1.5,3.5,5.5,7.5), c(3.5,3,2.5,5), circles = rep(0.4, 4), add = TRUE, bg = "red", inches = FALSE)
symbols(c(1.5,3.5,5.5), c(7.5,7,6.5), circles = rep(0.4, 3), add = TRUE, bg = "yellow", inches = FALSE)

par(bg = "slategray") #menu
text(4.5,5, "Four-in-a-Row")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2 || !(options[1] != options[2] && options[1] != options[3] && options[3] != options[2])){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- (options[1]+1) %% 12
    if(floor(click$y) == 2)
      options[2] <- (options[2]+1) %% 12
    if(floor(click$y) == 1)
      options[3] <- (options[3]+1) %% 12
    if(floor(click$y) == 0)
      options[4] <- (options[4]+1) %% 3
  }
  
  rect(1, 5, 7, 6, col = "yellow", border = "red")
  rect(1, 3, 7, 4, col = fiarColors[options[1]+1], border = "black")
  rect(1, 2, 7, 3, col = fiarColors[options[2]+1], border = "black")
  rect(1, 1, 7, 2, col = fiarColors[options[3]+1], border = "black")
  rect(1, 0, 7, 1, col = fiarColors[options[4]+4], border = "black")
  text(4,8, "Four-in-a-Row")
  text(4,3.5, "Player 1", col="gray30")
  text(4,2.5, "Player 2", col="gray30")
  text(4,1.5, "Board", col="gray30")
  text(4,0.5, ifelse(options[4]<2, ifelse(options[4],"CPU Second","Human"),"CPU First"))
  text(4,5.5, "Start")
  
  menu = min(menu + 1,2)
}

menu <- 0

#game loop
while(!checkwin(board) && any(board == 0)) {
  if(menu>0 && !checkwin(board)){
    selected <- floor(click$x)
    if(options[4]>0 && turn != options[4]){
      board <- doAI(board, options[4])
      turn <- 3 - turn
    } else if(selected >= 1 && selected <= 7){
      test <- dropp(selected, turn, board)
      if(!anyNA(test)){
        board <- test
        turn <- 3 - turn
      }
    }
  }
  
  if(options[4]>0)
    plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = ifelse(turn==options[4], "Human's turn", "Computer's turn"), ylab = "", axes = FALSE, frame.plot = FALSE)  
  else
    plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
  
  for(i in 1:7)
    for(j in 1:6)
      rect(i, -j + 8, i + 1, -j + 7, col = ifelse(board[j,i]==0, fiarColors[options[3]+1], ifelse(board[j,i]==1, fiarColors[options[1]+1], fiarColors[options[2]+1])), border = "black")
  
  menu <- 1
  if(!checkwin(board))
    click <- locator(1)
}

turn <- 3 - turn

plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
for(i in 1:7)
  for(j in 1:6)
    rect(i, -j + 8, i + 1, -j + 7, col = ifelse(board[j,i]==0, fiarColors[options[3]+1], ifelse(board[j,i]==1, fiarColors[options[1]+1], fiarColors[options[2]+1])), border = "black")

rect(2,2.5,7,6.5, col="white")
if(checkwin(board)){
  text(4.5,4, paste("Player ", turn))
  text(4.5,3, "wins!")
  polygon(c(3,3,4,5,6,7,7,3)/2+2, c(4,6,4.5,6,4.5,6,3,3)/2+3.25, col=fiarColors[options[turn+1]])
} else {
  text(4.5,3, "Draw!")
  polygon(c(3,4,5,6,7,6,8,5,4,3,4,3)/-3+7, c(3,2,4,3,4,5,10,6,7,6,5,3)/3+3, col="gray")
  polygon(c(3,4,5,6,7,6,8,5,4,3,4,3)/3+2, c(3,2,4,3,4,5,10,6,7,6,5,3)/3+3, col="gray")
}