par(bg = "slategray")
board <- matrix(0, nrow = 6, ncol = 7)
menu <- 0
click <- list(x = 3, y = 3)
options <- c(0, 1, 2)
turn <- 1
fiarColors <- c("red", "yellow", "blue", "green", "orange", "cyan", "purple", "white", "pink", "chartreuse", "tan", "darkorange4")

dropp <- function(col, player){
  for(r in 1:6)
    if(board[col,r]==0){
      board[col,r] <- player
      return(board)
    }
  return(NA)
}

checkwin <- function(){ 
  for(i in 1:6)
    if(any(apply(embed(board[i,], 4), 1, function(r) (all(r==r[1]) && r[1]!=0)))) #if all in a row subset are equal and not 0
      return(TRUE)
  for(i in 1:7)
    if(any(apply(embed(board[,i], 4), 2, function(r) (all(r==r[1]) && r[1]!=0)))) #columns
       return(TRUE)
  return(FALSE)
}

plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
par(bg = "slategray") #menu
text(5,5, "Four-in-a-Row")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2 || !(options[1] != options[2] && options[1] != options[3] && options[3] != options[2])){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] = (options[1]+1) %% 12
    if(floor(click$y) == 2)
      options[2] = (options[2]+1) %% 12
    if(floor(click$y) == 1)
      options[3] = (options[3]+1) %% 12
  }
  
  rect(1, 5, 7, 6, col = "yellow", border = "red")
  rect(1, 3, 7, 4, col = fiarColors[options[1]+1], border = "black")
  rect(1, 2, 7, 3, col = fiarColors[options[2]+1], border = "black")
  rect(1, 1, 7, 2, col = fiarColors[options[3]+1], border = "black")
  text(4,8, "Four-in-a-Row")
  text(4,3.5, "Player 1")
  text(4,2.5, "Player 2")
  text(4,1.5, "Board")
  text(4,5.5, "Start")
  
  menu = min(menu + 1,2)
}

menu <- 0

#game loop
while(!checkwin()) {
  if(menu>0 && !checkwin()){
    selected <- floor(click$x)
    if(selected >= 1 && selected <= 7){
      test <- dropp(selected, turn)
      if(!anyNA(test)){
        board <- test
        turn <- 3 - turn
      }
    }
  }
  
  plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
  
  for(i in 1:6)
    for(j in 1:7)
      rect(j, i, j + 1, i + 1, col = ifelse(board[j,i]==0, fiarColors[options[3]+1], ifelse(board[j,i]==1, fiarColors[options[1]+1], fiarColors[options[2]+1])), border = "black")
  
  menu <- 1
  if(!checkwin())
    click <- locator(1)
}

turn <- 3 - turn

plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
for(i in 1:6)
  for(j in 1:7)
    rect(j, i, j + 1, i + 1, col = ifelse(board[j,i]==0, fiarColors[options[3]+1], ifelse(board[j,i]==1, fiarColors[options[1]+1], fiarColors[options[2]+1])), border = "black")

rect(2,2.5,6,6.5, col="white")
text(4,4, paste("Player ", turn))
text(4,3, "wins!")
polygon(c(3,3,4,5,6,7,7,3)/2+1.5, c(4,6,4.5,6,4.5,6,3,3)/2+3.25, col=fiarColors[options[turn+1]])