par(bg = "gray85")
board <- matrix(0, nrow = 3, ncol = 3)
turn <- 1
menu <- 0
click <- list(x = 3, y = 3)
textboxes <- c(0,0)
options <- c(FALSE, 0, FALSE)
# Classic or Advanced
#   In Advanced, you can place an X, O, or Microboard in a square
#   If a microboard is 
# Colors: Red/Blue Orange/Cyan Purple/Yellow green/pink
# Computer or Human
btnColorsb <- c("red","orange", "purple", "green")
btnColorsf <- c("blue","cyan", "yellow", "pink2")

checkwin <- function(){
  if(any(apply(board, 1, function(r) (all(r==r[1]) && r[1]>0)))) #vertical
    return(TRUE)
  if(any(apply(board, 2, function(r) (all(r==r[1]) && r[1]>0)))) #horizontal
    return(TRUE)
  if(board[1,1] == board[2,2] && board[1,1] == board[3,3] && board[1,1] > 0) #primary diagonal
    return(TRUE)
  if(board[1,3] == board[2,2] && board[1,3] == board[3,1] && board[1,3] > 0) #secondary diagonal
    return(TRUE)
  return(FALSE)
}

#menu
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
lines(c(0.5,3,1.75,0.5,3), c(1,4,2.5,4,1), lwd=4, col="red")
symbols(7.5, 7.5, circles = 1, add = TRUE, bg = "transparent", fg="blue", inches = FALSE, lwd=4)
text(4.5,5, "Tic Tac Toe")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- !options[1]
    if(floor(click$y) == 2)
      options[2] <- (options[2]+1) %% 4
  }
  
  rect(1, 5, 7, 6, col = "white", border = "black")
  rect(1, 3, 7, 4, col = ifelse(options[1], "green", "red"), border = "black")
  rect(1, 2, 7, 3, col = btnColorsb[options[2]+1], border = "black")
  text(4,8, "Tic-Tac-Toe")
  text(4,3.5, "Advanced")
  text(4,2.5, "Colors", col=btnColorsf[options[2]+1])
  text(4,5.5, "Start")
  
  menu <- min(menu + 1,2)
}
plot(0, 0, type = "n", xlim = c(0, 3), ylim = c(0, 3), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
menu <- 0

#game loop
while(!checkwin() && any(board == 0)) {
  
  if(floor(click$x)<3 && floor(click$x)>-1){
    if(floor(click$y)<3 && floor(click$y)>-1){
      if(board[floor(click$x)+1, floor(click$y)+1] == 0){
        board[floor(click$x)+1, floor(click$y)+1] <- turn
        turn <- 3 - turn
      }
    }
  }
  
  #draw board
  plot(0, 0, type = "n", xlim = c(0, 3), ylim = c(0, 3), xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
  for(i in 1:2){
    lines(c(i,i),c(0,3), lwd=4)
    lines(c(0,3),c(i,i), lwd=4)
  }
  
  for(i in 1:3)
    for(j in 1:3)
      if(board[i,j] == 1){
        lines(c(i-0.8,i-0.2),c(j-0.8,j-0.2), col= btnColorsb[options[1]+1], lwd=4)
        lines(c(i-0.8,i-0.2),c(j-0.2,j-0.8), col= btnColorsb[options[1]+1], lwd=4)
      } else if(board[i,j] == 2){
        symbols(i-0.5, j-0.5, circles = 0.25, add = TRUE, bg = "transparent", fg=btnColorsf[options[1]+1], inches = FALSE, lwd=4)
      }
  
  menu <- 1
  
  click <- locator(1)
}
