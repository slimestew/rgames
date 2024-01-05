par(bg = "gray85")
boards <- array(0, dim = c(10, 3, 3))
sub <- 0
turn <- 1
menu <- 0
click <- list(x = 3, y = 3)
textboxes <- c(0,0)
options <- c(FALSE, 0, 0, FALSE)
# Classic or Advanced
#   In Advanced, you can place an X, O, or Microboard in a square
#   If a microboard is 
# Colors: Red/Blue Orange/Cyan Purple/Yellow green/pink
# Computer or Human  (computer can go first or second)
# Microboard Vision (can see the pieces on advanced microboards)
btnColorsb <- c("red","orange", "purple", "green")
btnColorsf <- c("blue","cyan", "yellow", "pink2")

checkwin <- function(b, sub){
  if(any(apply(b[sub,,], 1, function(r) (all(r==r[1]) && r[1]>0)))) #vertical
    return(max(apply(b[sub,,], 1, function(r) (ifelse((all(r==r[1]) && r[1]>0), r[1], 0))))) #checks for repeats unless -1
  if(any(apply(b[sub,,], 2, function(r) (all(r==r[1]) && r[1]>0)))) #horizontal
    return(max(apply(b[sub,,], 2, function(r) (ifelse((all(r==r[1]) && r[1]>0), r[1], 0))))) #max selects non zeroes
  if(boards[sub,1,1] == b[sub,2,2] && b[sub,1,1] == b[sub,3,3] && b[sub,1,1] > 0) #primary diagonal
    return(b[sub,1,1])
  if(boards[sub,1,3] == b[sub,2,2] && b[sub,1,3] == b[sub,3,1] && b[sub,1,3] > 0) #secondary diagonal
    return(b[sub,1,3])
  return(0)
}

getPiece <- function(sub, x, o){
  rect(0,0,4,4, col="#d9d9d980", border="transparent")
  lines(c(0.2,0.8), c(1.2,1.8), lwd=4, col=x)
  lines(c(0.2,0.8), c(1.8,1.2), lwd=4, col=x)
  symbols(1.5, 1.5, circles = 0.25, add = TRUE, bg = "transparent", fg=o, inches = FALSE, lwd=4)
  lines(c(1.2,1.8), c(0.5,0.8), lwd=4, col="darkorange4")
  lines(c(1.2,1.8), c(0.5,0.2), lwd=4, col="darkorange4")
  if(sub==0){
    lines(c(2.4,2.4),c(1.2,1.8), col="gray20", lwd=4)
    lines(c(2.6,2.6),c(1.2,1.8), col="gray20", lwd=4)
    lines(c(2.2,2.8),c(1.4,1.4), col="gray20", lwd=4)
    lines(c(2.2,2.8),c(1.6,1.6), col="gray20", lwd=4)
  }
  click <- locator(1)
  if(floor(click$y) < 1)
    return(0)
  if(floor(click$x) < 1)
    return(1)
  if(floor(click$x) < 2)
    return(2)
  if(floor(click$x) < 3 && sub==0)
    return(-1) 
  return(2)
}

drawBoard <- function(boards, sub, x, o){
  #draw board
  if(sub > 0){
    plot(0, 0, type = "n", xlim = c(0, 3), ylim = c(0, 3), xlab = paste("Player ", turn, "'s turn, Board ", sub, sep=""), ylab = "Back", axes = FALSE, frame.plot = FALSE)
  } else {
    plot(0, 0, type = "n", xlim = c(0, 3), ylim = c(0, 3), xlab = paste("Player ", turn, "'s turn", sep=""), ylab = "", axes = FALSE, frame.plot = FALSE)
  }
  
  for(i in 1:2){
    lines(c(i,i),c(0,3), lwd=4)
    lines(c(0,3),c(i,i), lwd=4)
  }
  
  for(i in 1:3)
    for(j in 1:3)
      if(boards[sub+1,i,j] == 1){
        lines(c(i-0.8,i-0.2),c(j-0.8,j-0.2), col=x, lwd=4)
        lines(c(i-0.8,i-0.2),c(j-0.2,j-0.8), col=x, lwd=4)
      } else if(boards[sub+1,i,j] == 2){
        symbols(i-0.5, j-0.5, circles = 0.25, add = TRUE, bg = "transparent", fg=o, inches = FALSE, lwd=4)
      } else if(boards[sub+1,i,j] == -1){
        lines(c(i-0.4,i-0.4),c(j-0.8,j-0.2), col="gray20", lwd=4)
        lines(c(i-0.6,i-0.6),c(j-0.2,j-0.8), col="gray20", lwd=4)
        lines(c(i-0.2,i-0.8),c(j-0.4,j-0.4), col="gray20", lwd=4)
        lines(c(i-0.2,i-0.8),c(j-0.6,j-0.6), col="gray20", lwd=4)
        #TODO: inner symbols
      }
}

doAI <- function(b, gamemode, turnorder){
  
  if(gamemode==1){ #advanced
    return(b)
  }
  
  #basic
  for(i in 1:3){ # offense
    for(j in 1:3){
      if(b[1,i,j] == 0){
        b[1,i,j] <- turnorder
        if(checkwin(b, 1)){
          return(b)
        } else {
          b[1,i,j] <- 0
        }
      }
    }
  }
  
  for(i in 1:3){ # defense
    for(j in 1:3){
      if(b[1,i,j] == 0){
        b[1,i,j] <- 3 - turnorder #check enemy move
        if(checkwin(b, 1)){ #if enemy wins
          b[1,i,j] <- turnorder #place my own
          return(b)
        } else {
          b[1,i,j] <- 0
        }
      }
    }
  }
  
  temp <- expand.grid(1:3, 1:3)[sample(9), ] # random
  for(i in 1:9)
    if(b[1,temp[i,1], temp[i,2]] == 0){
      b[1,temp[i,1],temp[i,2]] <- turnorder
      return(b)
    }

  # should never happen
  return(b)
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
    if(floor(click$y) == 1)
      options[3] <- (options[3]+1) %% 3
    if(floor(click$y) == 0 && options[1])
      options[4] <- !options[4]
  }
  
  rect(1, 5, 7, 6, col = "white", border = "black")
  rect(1, 3, 7, 4, col = ifelse(options[1], "green", "red"), border = "black")
  rect(1, 2, 7, 3, col = btnColorsb[options[2]+1], border = "black")
  rect(1, 1, 7, 2, col = ifelse(options[3], "green", "red"), border = "black")
  if(options[1])
    rect(1, 0, 7, 1, col = ifelse(options[4], "green", "red"), border = "black")
  text(4,8, "Tic-Tac-Toe")
  text(4,3.5, "Advanced")
  text(4,2.5, "Colors", col=btnColorsf[options[2]+1])
  text(4,1.5, ifelse(options[3]<2, ifelse(options[3],"CPU Second","Human"),"CPU First"))
  text(4,0.5, ifelse(options[1],"Microboard Vision", ""))
  text(4,5.5, "Start")
  
  menu <- min(menu + 1,2)
}
plot(0, 0, type = "n", xlim = c(0, 3), ylim = c(0, 3), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
menu <- 0

#game loop
while(!checkwin(boards, 1)!=0 && (any(boards[1,,] == 0) || any(boards[1,,] == -1))) {
  
  if(options[3]==turn){
    boards <- doAI(boards, options[1], options[3])
    turn <- 3 - turn
  }
  
  if(options[1]){ #advanced
    
    if(floor(click$x)<0) #back button
      sub <- 0
    
    if(floor(click$x)<3 && floor(click$x)>-1 && floor(click$y)<3 && floor(click$y)>-1 && !checkwin(boards,1)){ #on the board
      
      if(boards[sub+1, floor(click$x)+1, floor(click$y)+1] == 0){ #new piece
        boards[sub+1, floor(click$x)+1, floor(click$y)+1] <- getPiece(sub, btnColorsb[options[2]+1], btnColorsf[options[2]+1]) #get piece
        
        if(boards[sub+1, floor(click$x)+1, floor(click$y)+1] != -1){
          if(boards[sub+1, floor(click$x)+1, floor(click$y)+1] != 0){
            sub <- -1
            turn <- 3 - turn #end turn
          }
          
        } else {
          sub <- floor(click$x)+1 + (3*(floor(click$y))) #enter microboard
          
          while(TRUE){ #place second symbol
            drawBoard(boards, sub, btnColorsb[options[2]+1], btnColorsf[options[2]+1])
            click <- locator(1)
            if(floor(click$x)<3 && floor(click$x)>-1 && floor(click$y)<3 && floor(click$y)>-1){
              boards[sub+1, floor(click$x)+1, floor(click$y)+1] <- getPiece(sub, btnColorsb[options[2]+1], btnColorsf[options[2]+1]) #get piece
              if(boards[sub+1, floor(click$x)+1, floor(click$y)+1] != 0){
                sub <- -1
                turn <- 3 - turn
                break
              }
            }
          }
        
        }
        
      } #new piece
      
      if(boards[1, floor(click$x)+1, floor(click$y)+1] == -1 && sub == 0 && !checkwin(boards, 1)) #enter microboard
        sub <- floor(click$x)+1 + (3*(floor(click$y)))
      
      if(sub==-1) #fixes a bug where placing a piece inside of a 
        sub <- 0
      
    } #on the board
    
    for(i in 2:10){ #check microboards for a win
      temp <- checkwin(boards, i)
      if(temp!=0)
        boards[1, (i-2)%%3+1, floor((i-2)/3)+1] <- temp #replace with a piece
    }
    
  } else { #basic
    if(floor(click$x)<3 && floor(click$x)>-1 && floor(click$y)<3 && floor(click$y)>-1){ #on the board
      if(boards[1, floor(click$x)+1, floor(click$y)+1] == 0){ #new piece   
        boards[1, floor(click$x)+1, floor(click$y)+1] <- turn
        turn <- 3 - turn
      }
    }
  }
  
  drawBoard(boards, sub, btnColorsb[options[2]+1], btnColorsf[options[2]+1])
  
  menu <- 1
  
  if(!checkwin(boards, 1)!=0 && (any(boards[1,,] == 0) || any(boards[1,,] == -1)))
    click <- locator(1)
} #game loop

turn <- 3 - turn
drawBoard(boards, sub, btnColorsb[options[2]+1], btnColorsf[options[2]+1])

rect(1,1,2,2, col="#d9d9d980", border="transparent")
if(!options[3] || turn==options[3]){
  if(checkwin(boards, 1)!=0)
    text(1.5,1.5, "You win!")
  else
    text(1.5,1.5, "Draw")
} else {
  text(1.5,1.5, "Game Over")
}
