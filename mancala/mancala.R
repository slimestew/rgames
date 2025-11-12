par(bg = "goldenrod2")
board <- rep(4, 14)
board[7] <- 0
board[14] <- 0
menu <- 0
click <- list(x = 3, y = 3)
options <- c(1, 0, 0)
# Theme
# 4 row board? Y/N
# Human or Computer (computer can go first or second)
turn <- 1

drawBoard <- function(board, turn, variation){
  plot(0, 0, type = "n", xlim = c(1, 9), ylim = c(1, 7), col="white", xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
  if(variation){
    for (i in 1:7) {
      for (j in 1:4) {
        rect(i, j+(j>2)*2, i+1, j+1+(j>2)*2, col = "goldenrod2", border = "black")
      }
    }
  } else {
    rect(1, 1, 2, 7, col = "goldenrod2", border = "black")
    rect(8, 1, 9, 7, col = "goldenrod2", border = "black")
    for (i in 2:7) {
      for (j in 0:1) {
        # todo: 2p is backwards, flip right to left
        yOffset <- (j * 4)
        xOffset <- (-2*j+1)*i
        house <- i+(j*7)-1
        symbols(x = xOffset + (0.5+(j*9)), y = yOffset + 2, circles = 0.5, inches = FALSE, bg = "goldenrod2", fg = "black", add = TRUE)
        text(xOffset + (0.5+(j*9)), (j*2)+3, board[house])
        if(board[house] %% 2 == 1 || board[house] >= 9){
          symbols(x = xOffset + (0.5+(j*9)), y = yOffset + 2, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
        }
        if(board[house] >= 2){
          symbols(x = xOffset + (0.25+(j*9)), y = yOffset + 1.75, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
          symbols(x = xOffset + (0.75+(j*9)), y = yOffset + 2.25, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
        }
        if(board[house] >= 4){
          symbols(x = xOffset + (0.75+(j*9)), y = yOffset + 1.75, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
          symbols(x = xOffset + (0.25+(j*9)), y = yOffset + 2.25, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
        }
        if(board[house] >= 6){
          symbols(x = xOffset + (0.25+(j*9)), y = yOffset + 2, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
          symbols(x = xOffset + (0.75+(j*9)), y = yOffset + 2, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
        }
        if(board[house] >= 8){
          symbols(x = xOffset + (0.5+(j*9)), y = yOffset + 1.75, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
          symbols(x = xOffset + (0.5+(j*9)), y = yOffset + 2.25, circles = 0.125, inches = FALSE, bg = "black", fg = "black", add = TRUE)
        }
      }
    }
    text(1.5, 4.5, board[14])
    text(8.5, 3.5, board[7])
  #rectangles for stores
  #large circles for pits
  #symbol circles for pieces (center circle for 1, .' for 2, :- for 3, :: for 4, :-: for 5, any more is 6 pips like a die)
  }
}

checkwin <- function(board){
  if(length(board) == 14)
    return(all(board[1:6] == 0) || all(board[8:13] == 0))
  return(all(board[1:14] == 0) || all(board[15:28] == 0))
}

doAI <- function(board, human){
  bestScore <- 0
  bestOpp <- 0
  bestMove <- 0
  tempboard <- board
  depth <- 5 #depth can be improved to make it better i guess?
  ownScore <- 7
  if(human == 2)
    ownScore <- 14
  for (store in 2:7) {
    if(board[3*store + 16*(3-human) - 2*(store*(3-human)) - 17] == 0) #same formula but for flipped turns
      next
    tempBoard <- iteratemove(board, 3 - human, 3 - human, depth)[[1]]  #option is 1 if cpu goes second, so the turn will be 2
      if( (tempBoard[ownScore] > bestScore) || (tempBoard[ownScore] == bestScore && tempBoard[21 - ownScore] < bestOpp)){
        #beats previous best score, or ties while limiting opponent's score
        bestScore <- tempBoard[ownScore]
        bestOpp <- tempBoard[21 - ownScore]
        bestMove <- store
      }
      tempboard <- board
  }
  #seems to break as player 1, but works fine on p2
  return(dropp(bestMove, 3 - human, board))
}

iteratemove <- function(board, side, turn, depth){
  bestScore <- 0
  bestOpp <- 0
  bestMove <- 0
  tempboard <- board
  dropped <- c(0,0)
  ownScore <- 7
  if(side == 2){
    ownScore <- 14
  }
  for (store in 2:7) {
    dropped <- dropp(store, turn, board)
    if(any(is.na(dropped)))
      return(list(dropped[1:14], side, dropped[15], depth-1))
    if(depth>1){
      tempBoard <- iteratemove(dropped[1:14], side, dropped[15], depth-1)[[1]]
      if((tempBoard[ownScore] > bestScore) || (tempBoard[ownScore] == bestScore && tempBoard[21 - ownScore] < bestOpp)){
        #beats previous best score, or ties while limiting opponent's score
        bestScore <- tempBoard[ownScore]
        bestOpp <- tempBoard[21 - ownScore]
        bestMove <- store
      }
    }
  }
  return(list(dropped[1:14], side, dropped[15], depth))
}

dropp <- function(selected, turn, board){
  index <- 3*selected + 16*turn - 2 * (selected*turn) - 17 #selected is offset by 1 due to p2's store
  if(board[index] == 0)
    board[index] <- NA
  else{
    temp <- board[index]
    board[index] <- 0
    endingcell <- ((index+temp-1) %% 14) + 1
    
    for(i in index:(index+temp-1)){
      if(!((turn == 1 && i == 7) && (turn == 2 && i == 14))) #skip opponent's store
        board[(i %% 14) + 1] <- board[(i %% 14) + 1] + 1
    } 
    
    if((turn == (endingcell>7)+1) && (board[endingcell] == 1) && (board[-1*endingcell+14] > 0) && (endingcell %% 7 > 0)){ #previously empty pit on on player's side
      board[endingcell] <- 0 #clear your pit
      board[turn*7] <- board[turn*7] + board[-1*endingcell+14] + 1 # add opposing pit and your pit to store
      board[-1*endingcell+14] <- 0 #clear opposing pit
    }
    
    if(!((endingcell == 7 && turn == 1) || (endingcell == 14 && turn == 2))) #last move was store
      turn <- 3 - turn
  }
  
  return(c(board, turn))
}

plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
symbols(c(2,3,4,5,6,7,2,3,4,5,6,7), c(rep(7,6),rep(2,6)), circles = rep(0.4, 12), add = TRUE, bg = "yellow", inches = FALSE)
symbols(c(1,8), rep(4.5,2), circles = rep(0.8, 2), add = TRUE, bg = "yellow", inches = FALSE)

par(bg = "goldenrod2") #menu
text(4.5,5, "Mancala")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2 ){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- (options[1]+1) %% 4
    if(floor(click$y) == 2)
      options[2] <- (options[2]+1) %% 2
    if(floor(click$y) == 1)
      options[3] <- (options[3]+1) %% 3
  }
  
  rect(1, 5, 7, 6, col = "yellow", border = "pink4")
  rect(1, 3, 7, 4, col = "goldenrod4", border = "black")
  rect(1, 2, 7, 3, col = "goldenrod1", border = "black")
  rect(1, 1, 7, 2, col = "goldenrod3", border = "black")
  text(4,8, "Mancala")
  text(4,3.5, "Theme")
  text(4,2.5, "4 Row Board")
  text(4,1.5, ifelse(options[3]<2, ifelse(options[3],"CPU Second","Human"),"CPU First"))
  text(4,5.5, "Start")
  
  menu = min(menu + 1,2)
}

menu <- 0

#game loop
while(!checkwin(board)) {
  if(menu>0 && !checkwin(board)){
    selected <- floor(click$x)
    if(options[3]>0 && turn != options[3]){
      aiStuff <- doAI(board, options[3])
      board <- aiStuff[1:14]
      turn <- aiStuff[15]
    } else if(selected > 1 && selected < 8){
      test <- dropp(selected, turn, board)
      if(!anyNA(test[1:14])){
        board <- test[1:14]
        turn <- test[15]
      }
    }
  }
  
  if(options[3]>0)
    plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = ifelse(turn==options[4], "Human's turn", "Computer's turn"), ylab = "", axes = FALSE, frame.plot = FALSE)  
  else
    plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
  
  drawBoard(board, turn, options[2])
  
  menu <- 1
  if(!checkwin(board))
    click <- locator(1)
}

if(options[2] == 0) {
  board[7] <- board[7]+sum(board[1:6])
  board[14] <- board[14]+sum(board[8:13])
  board[c(1:6, 8:13)] <- 0
}

drawBoard(board, turn, options[2])
turn <- 3 - turn
rect(2,2.5,7,6.5, col="white")

if(board[7] == board[14]) {
  text(4.5,3, "Draw!")
  polygon(c(3,4,5,6,7,6,8,5,4,3,4,3)/-3+7, c(3,2,4,3,4,5,10,6,7,6,5,3)/3+3, col="gray")
  polygon(c(3,4,5,6,7,6,8,5,4,3,4,3)/3+2, c(3,2,4,3,4,5,10,6,7,6,5,3)/3+3, col="gray")
} else {
  text(4.5,4, paste("Player ", (board[7]<board[14]) + 1))
  text(4.5,3, "wins!")
  polygon(c(3,3,4,5,6,7,7,3)/2+2, c(4,6,4.5,6,4.5,6,3,3)/2+3.25, col="yellow")
}