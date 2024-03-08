par(bg = "thistle3")
board <- list(c(), c(), c())
menu <- 0
selected <- 0
click <- list(x = 3, y = 3)
textboxes <- c(0,0)
options <- c(0, 0, 0)
colorsb <- c("tan", "lightsalmon1", "lemonchiffon", "khaki2", "wheat3")
colorsf <- c("sienna","mistyrose4", "tomato1", "darkseagreen4", "lightsteelblue4")
colorst <- c()
optext <- c("Light Color", "Dark Color", "Alternating", "Inverse Alt")

checkwin <- function(board, options){
  return(all(diff(board[3]) >= 0) && length(board[[3]]) == options[3]+3)
}

moveBlock <- function(board, temp, selected){
  if(any(!is.na(board[[temp]])) && any(!is.na(board[[selected]])) &&
     (board[[temp]][1] > board[[selected]][1])) #invalid move
    return(board)
  board[[temp]] <- c(board[[selected]][1], board[[temp]])
  board[[selected]] <- board[[selected]][-1]
  return(board)
}

drawBoard <- function(board, colorst, textboxes, maximum){
  #max <- sum(sapply(board, length))
  denom <- ceiling(maximum*2.5)
  plot(0, 0, type = "n", xlim = c(0, 3), ylim = c(0, 10), xlab = paste("Guesses:",textboxes[1]), ylab = paste("Minimum:",textboxes[2]), axes = FALSE, frame.plot = FALSE)
  rect(0,0,3,-1, col=colorst[1], fg="black")
  
  for(i in 0:2) #rods
    rect((0.4+i),0,(0.6+i),10, col=colorst[1], fg="black")
  
  for(i in 1:3) #disks
    if(length(board[[i]])>0)
      for(j in 1:length(board[[i]]))
        rect((board[[i]][j]/denom)+i-1, length(board[[i]])-j, (1-board[[i]][j]/denom)+i-1, length(board[[i]])-j+1, col=ifelse((board[[i]][j] %% 2 == 0), colorst[1], colorst[2]))
}


#menu
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
text(4.5,5, "The Tower of Hanoi")
rect(1,1,8,2, col="tan")
rect(2,2.5,7,3.5, col="sienna")
rect(3,6,6,7, col="tan")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- (options[1]+1) %% 4
    if(floor(click$y) == 2)
      options[2] <- (options[2]+1) %% 5
    if(floor(click$y) == 1)
      options[3] <- (options[3]+1) %% 7

  }
  
  rect(1, 5, 7, 6, col = "white", border = "black")
  rect(1, 3, 7, 4, col = ifelse(options[1]>1, "green", "red"), border = "black")
  rect(1, 2, 7, 3, col = colorsb[options[2]+1], border = "black")
  rect(1, 1, 7, 2, col = "sienna1")
  text(4,8, "Tower of Hanoi")
  text(4,3.5, optext[options[1]+1])
  text(4,2.5, "Colors", col=colorsf[options[2]+1])
  text(4,1.5, options[3]+3)
  text(4,5.5, "Start")
  
  menu <- min(menu + 1,2)
}

board[1] <- list(c((options[3]+3):1))
textboxes[2] <- 2^(options[3]+3)-1
menu <- 0

if(options[1] %% 2 == 0){
  colorst[1] <- colorsb[options[2]+1]
} else {
  colorst[1] <- colorsf[options[2]+1]
}
if(options[1] %% 3 == 0){
  colorst[2] <- colorsb[options[2]+1]
} else {
  colorst[2] <- colorsf[options[2]+1]
}

#game loop
while(!checkwin(board, options)) {
  if(menu>0){
    if(selected == 0){
      selected <- floor(click$x) + 1
      if(floor(click$x) < 0)
        selected <- 1
      if(floor(click$x) > 3)
        selected <- 3
    } else {
      temp <- floor(click$x) + 1
      if(floor(click$x) < 0)
        temp <- 1
      if(floor(click$x) > 3)
        temp <- 3
      
      if(temp != selected){
        board <- moveBlock(board, temp, selected)
        textboxes[1] <- textboxes[1]+1
      }
      selected <- 0
      temp <- 0
    }
    
  }
  
  drawBoard(board, colorst, textboxes, options[3]+3)
  
  menu <- 1
  
  if(!checkwin(boards, options))
    click <- locator(1)
} #game loop

rect(1,3.5,2,5.5, col="thistle4")
if(textboxes[1] == textboxes[2]){
  text(1.5,4.5, "You win!")
} else {
  text(1.5,4.5, "Nice try!")
}