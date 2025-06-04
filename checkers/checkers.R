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

checkWin <- function(board){
  return(sum(abs(board) == 1) == 0 || sum(abs(board) == 2) == 0 )
}

doAI <- function(board){
  return(board)
}

checkCap <- function(board, turn){
  order <- c(-1,1)
  for(i in 1:8){
    for(j in 1:8){
      for(k in order)
      if(abs(board[i,j]) == turn){
        if(board[i,j] < 0){ #king
          if(i+k < 1 || i+k > 8 || j+k < 1 || j+k > 8) #oob
            break
          if(board[i+k,j+k] == (3-turn)){ #opposing piece
            if(i+k+k < 1 || i+k+k > 8 || j+k+k < 1 || j+k+k > 8) #oob
              break
            if(board[i+k+k,j+k+k] == 0) #empty
              return(TRUE)
          }
        } else { #man
          if(i+k < 1 || i+k > 8 || j+order[turn] < 1 || j+order[turn] > 8) #oob
            break
          if(board[i+k,j+order[turn]] == (3-turn)){ #opposing piece
            if(i+k+k < 1 || i+k+k > 8 || j+order[turn]+order[turn] < 1 || j+order[turn]+order[turn] > 8) #oob
              break
            if(board[i+k+k,j+order[turn]+order[turn]] == 0) #empty
              return(TRUE)
          }
        }
      }
    }
  }
  return(FALSE)
}

movePiece <- function(board, selected, turn){
  tempBoard <- board
  if(checkCap(board,turn)){ #capture required
    if((abs(selected[1] - selected[3]) == 2) && (abs(selected[2] - selected[4]) == 2) #capture
       && (tempBoard[selected[1], selected[2]] == turn && tempBoard[selected[3], selected[4]] == 0) #open space
       && ((tempBoard[(selected[1]+selected[3])/2, (selected[2]+selected[4])/2] == 3- turn))){ #enemy
      tempiece <- tempBoard[selected[1], selected[2]] #swap
      tempBoard[selected[1], selected[2]] <- tempBoard[selected[3], selected[4]]
      tempBoard[selected[3], selected[4]] <- tempiece
      tempBoard[(selected[1]+selected[3])/2, (selected[2]+selected[4])/2] <- 0 #enemy
      if((selected[4] == 1 && tempBoard[selected[3], selected[4]] == 1) || (selected[4] == 8 && tempBoard[selected[3], selected[4]] == 2))
         tempBoard[selected[3], selected[4]] <- tempBoard[selected[3], selected[4]]*-1
    }
    return(tempBoard) #if just here, failed
  }
  if(abs(selected[1] - selected[3]) == 1 && abs(selected[2] - selected[4]) == 1 && tempBoard[selected[3], selected[4]] == 0) #no caps
    tempBoard[c(selected[1], selected[3]), c(selected[2], selected[4])] <- tempBoard[c(selected[3], selected[1]), c(selected[4], selected[2])] #swap
  return(tempBoard)
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
      if(board[i,j]!=0)
        symbols(gridsize-i+1.5, gridsize-j+1.5, circles = 0.25, add = TRUE, fg = themeColors[[options[1]+1]][3-board[i,j]], bg = themeColors[[options[1]+1]][board[i,j]], inches = FALSE, lwd=2)
      if(board[i,j]<0)
        symbols(gridsize-i+1.5, gridsize-j+1.5, circles = 0.1, add = TRUE, fg = themeColors[[options[1]+1]][-1*board[i,j]], bg = themeColors[[options[1]+1]][3-(-1*board[i,j])], inches = FALSE, lwd=4)
    }
  } else {
    for(i in 1:gridsize)
      for(j in 1:gridsize){
        rect(i, j, i + 1, j+1, col = themeColors[[options[1]+1]][(1 - (j%%2 + i%%2) %% 2) + 3], border = "black")
        if(board[i,j]!=0)
          symbols(i+0.5, j+0.5, circles = 0.25, add = TRUE, fg = themeColors[[options[1]+1]][3-abs(board[i,j])], bg = themeColors[[options[1]+1]][abs(board[i,j])], inches = FALSE, lwd=2)
        if(board[i,j]<0)
          symbols(i+0.5, j+0.5, circles = 0.1, add = TRUE, fg = themeColors[[options[1]+1]][-1*board[i,j]], bg = themeColors[[options[1]+1]][3-(-1*board[i,j])], inches = FALSE, lwd=4)
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
while(!checkWin(board) || floor(click$y) < 2) {
  
  if(menu>0 && !checkWin(board)){
    
    if(any(selected == -1)){ #select piece
      selected <- c(floor(click$x), floor(click$y))
      if(flipboard)
        selected <- c(gridsize-selected[1]+1, gridsize-selected[2]+1)
      if(!is.na(board[selected[1],selected[2]]) && board[selected[1],selected[2]]!=turn) #deselect piece
        selected <- c(-1, -1)
    } else { #move piece
      if((!(floor(click$x) == selected[1] && floor(click$y) == selected[2]) && !flipboard))
        temp <- movePiece(board, c(selected, floor(click$x), floor(click$y)), turn)
      if((!(gridsize-floor(click$x)+1 == selected[1] && gridsize-floor(click$y)+1 == selected[2]) && flipboard))
        temp <- movePiece(board, c(selected, gridsize-floor(click$x)+1, gridsize-floor(click$y)+1), turn)
      if(!identical(temp,board)){ #game state changed
          if(!checkCap(board,turn)){
            turn <- 3 - turn
            pause <- TRUE
          }
        board <- temp
      }
      selected <- c(-1, -1) #clear for next turn
    }
    
    if(options[3]>0 && turn != options[3]){
      board <- doAI(board, options[4])
      turn <- 3 - turn
      pause <- TRUE
    }
  }
  
  if(!pause){
    drawBoard(board, options, turn, selected, flipboard)
  } else {
    plot(0, 0, type = "n", xlim = c(0, gridsize+1), ylim = c(0, gridsize+1), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
    if(!checkWin(board))
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

rect(2,2.5,7,6.5, col="white")
if(checkWin(board)){
  text(4.5,4, paste("Player ", turn))
  text(4.5,3, "wins!")
  polygon(c(3,3,4,5,6,7,7,3)/2+2, c(4,6,4.5,6,4.5,6,3,3)/2+3.25, col=fiarColors[options[turn+1]])
}