par(bg = "mediumpurple")
board <- matrix(0, nrow = 8, ncol = 8)
menu <- 0
click <- list(x = 3, y = 3)
selected <- c(-1, -1)
options <- c(0, FALSE, 0)
# Theme
# Draughts
#     Draughts is played on a 10x10 board
# Human or Computer (computer can go first or second)
turn <- 1
flipboard <- TRUE
pause <- FALSE
pieces <- c("⛀", "⛁", "⛂", "⛃")
themes <- c("Monochrome", "Color", "Classical", "Consortium") #last uses Unicode
themeColors <- list(c("white","black","gray80","gray10"), c("white","red","wheat","seagreen"), c("lightyellow","#402010","tan","sienna"), c("white","black","gray80","gray10"))

checkwin <- function(board){ 
  return(FALSE)
}

doAI <- function(board){
  return(board)
}

movePiece <- function(board, selected, turn){
  
  return(board)
}

drawBoard <- function(board, options, turn, selected, flipboard){
  gridsize <- 8
  if(options[2])
    gridsize <- 10
  
  if(options[3]>0)
    plot(0, 0, type = "n", xlim = c(1, gridsize+1), ylim = c(1, gridsize+1), col="white", xlab = ifelse(turn==options[4], "Human's turn", "Computer's turn"), ylab = "", axes = FALSE, frame.plot = FALSE)  
  else
    plot(0, 0, type = "n", xlim = c(1, gridsize+1), ylim = c(1, gridsize+1), col="white", xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if(flipboard){
  for(i in 1:gridsize)
    for(j in 1:gridsize){
      rect(gridsize-i+1, gridsize-j+1, gridsize-i+2, gridsize-j+2, col = themeColors[[options[1]+1]][(1 - (j%%2 + i%%2) %% 2) + 3], border = "black")
      if(board[i,j]>0)
        symbols(gridsize-i+1.5, gridsize-j+1.5, circles = 0.25, add = TRUE, fg = themeColors[[options[1]+1]][3-board[i,j]], bg = themeColors[[options[1]+1]][board[i,j]], inches = FALSE, lwd=2)
    }
} else {
  for(i in 1:gridsize)
    for(j in 1:gridsize){
      rect(i, j, i + 1, j+1, col = themeColors[[options[1]+1]][(1 - (j%%2 + i%%2) %% 2) + 3], border = "black")
      if(board[i,j]>0)
        symbols(i+0.5, j+0.5, circles = 0.25, add = TRUE, fg = themeColors[[options[1]+1]][3-board[i,j]], bg = themeColors[[options[1]+1]][board[i,j]], inches = FALSE, lwd=2)
    }
  }
  rect(0, 0, 1, 1, col = "thistle", border = "black", xpd = TRUE)
  if(all(selected > 0)){
    if(flipboard){
      rect(gridsize-selected[1]+1, gridsize-selected[2]+1, gridsize-selected[1]+2, gridsize-selected[2]+2, col = "transparent" , border = "mediumpurple4", lwd = 3)
    } else {
      rect(selected[1], selected[2], selected[1]+1, selected[2]+1, col = "transparent" , border = "mediumpurple4", lwd = 3)
    }
  }
  
}

plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="mediumpurple", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
symbols(c(1.5,3.5,5.5,7.5), c(3.5,3,2.5,5), circles = rep(0.4, 4), add = TRUE, bg = "white", inches = FALSE)
symbols(c(1.5,3.5,5.5), c(7.5,7,6.5), circles = rep(0.4, 3), add = TRUE, bg = "black", inches = FALSE)

par(bg = "mediumpurple") #menu
text(4.5,5, "Checkers")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- (options[1]+1) %% 4
    if(floor(click$y) == 2)
      options[2] <- !options[2]
    if(floor(click$y) == 1)
      options[3] <- (options[3]+1) %% 3
  }
  
  rect(1, 5, 7, 6, col = "thistle", border = "darkorchid4")
  rect(1, 3, 7, 4, col = themeColors[[options[1]+1]][4], border = "black")
  rect(1, 2, 7, 3, col = ifelse(options[2], "seagreen", "red"), border = "black")
  rect(1, 1, 7, 2, col = "azure", border = "black")
  text(4,8, "Checkers")
  text(4,3.5, themes[options[1]+1], col=themeColors[[options[1]+1]][3])
  text(4,2.5, "Draughts")
  text(4,1.5, ifelse(options[3]<2, ifelse(options[3],"CPU Second","Human"),"CPU First"))
  text(4,5.5, "Start")
  
  menu = min(menu + 1,2)
}

if(options[2])
  board <- matrix(0, nrow = 10, ncol = 10)

gridsize <- nrow(board)
for(i in 1:gridsize){
  for(j in 1:(gridsize-2)/2){
    if(((j%%2 + i%%2) %% 2) == 0){
      board[i,j] <- 2
    }
    if(((j%%2 + i%%2) %% 2) == 1){
      board[i,gridsize-j+1] <- 1
    }
  }
}

menu <- 0

#game loop
while(!checkwin(board) || floor(click$y) < 2) {
  
  if(menu>0 && !checkwin(board)){
    
    if(any(selected == -1)){
      selected <- c(floor(click$x), floor(click$y))
      if(flipboard)
        selected <- c(gridsize-selected[1]+1, gridsize-selected[2]+1)
      if(!is.na(board[selected[1],selected[2]]) && board[selected[1],selected[2]]!=turn)
        selected <- c(-1, -1)
    }else{
      if((!(floor(click$x) == selected[1] && floor(click$y) == selected[2]) && !flipboard) ||
         (!(gridsize-floor(click$x)+1 == selected[1] && gridsize-floor(click$y)+1 == selected[2]) && flipboard) ){
        board <- movePiece(board, c(selected, floor(click$x), floor(click$y)), turn)
        turn <- 3 - turn
        pause <-TRUE
      }
      selected <- c(-1, -1)
    }
    
    if(options[3]>0 && turn != options[3]){
      board <- doAI(board, options[4])
      turn <- 3 - turn
      pause <-TRUE
    }
  }
  
  if(!pause){
    
    drawBoard(board, options, turn, selected, flipboard)
    
  } else {
    
    plot(0, 0, type = "n", xlim = c(0, gridsize+1), ylim = c(0, gridsize+1), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
    
    if(!checkwin(board))
      text(4,4, paste("Click for Player",turn))
    
    flipboard <- FALSE
    if(turn == 1)
      flipboard <- TRUE
    
    menu <- -1
  }
  
  menu <- min(menu+1, 1)
  
  click <- locator(1)
  
  if(pause)
    pause <- FALSE
}

turn <- 3 - turn

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