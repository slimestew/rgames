par(bg = "slategray3")
board <- matrix(0, nrow = 5, ncol = 5)
menu <- 0
click <- list(x = 3, y = 3)
options <- c(0, 0, 0)
# Theme
# Freeplay
# Difficulty
themeColors <- c(
  list(c("slateblue4", "slategray2")), list(c("blue4", "darkslategray2")), list(c("gray5", "white"))
)

fliplights <- function(x, y, board){
  board[y, x] <- 1 - board[y, x]
  if(y < 5)
    board[y+1, x] <- 1 - board[y+1, x]
  if(y > 1)
    board[y-1, x] <- 1 - board[y-1, x]
  if(x < 5)
    board[y, x+1] <- 1 - board[y, x+1]
  if(x > 1)
    board[y, x-1] <- 1 - board[y, x-1]
  return(board)
}


plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)

par(bg = "slategray3") #menu
text(1.5,8, "Lights Out")
polygon(c(4,4,3,3.5,5,6.5,7,6,6), c(1,2,5,7,8,7,5,2,1), col="lightgoldenrod1", border="black")
rect(4,0,6,1, col = "gray30", border = "black")
segments(c(4.5, 5.5, 4.5, 5.5, 4, 6, 5, 5,  6, 6, 5), c(1, 1, 4, 4, 5, 5, 5, 5,  1, 0.5, 1), c(4.5, 5.5, 4, 6, 5, 5, 4.75, 5.25,  4, 5, 4), c(4, 4, 5, 5, 4, 4, 4.25, 4.25,  0, 0, 0.5), col="black")


while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- (options[1]+1) %% 3
    if(floor(click$y) == 2)
      options[2] <- (options[2]+1) %% 2
    if(floor(click$y) == 1)
      options[3] <- (options[3]+1) %% 5
  }
  
  rect(1, 5, 7, 6, col = "gray30", border = "white")
  rect(1, 3, 7, 4, col = themeColors[[options[1]+1]][1], border = "black")
  rect(1, 2, 7, 3, col = ifelse(options[2], "green", "gray40"), border = "black")
  rect(1, 1, 7, 2, col = "gray60", border = "gray20")
  text(4,8, "Lights Out")
  text(4,3.5, "Theme", col=themeColors[[options[1]+1]][2])
  text(4,2.5, "Freeplay", col="black")
  text(4,1.5, paste("Difficulty ", options[3]+1), col="black")
  text(4,5.5, "Start")
  
  menu = min(menu + 1,2)
}

menu <- 0
moves <- list()
if(!options[2]){
  for(i in 1:((options[3]+1)*2 + 1)){
    moves <- c(moves, list(c(sample(1:5, 1), sample(1:5, 1))))
    while(any(duplicated(moves) == TRUE)){
      moves <- moves[-length(moves)]
      moves <- c(moves, list(c(sample(1:5, 1), sample(1:5, 1))))
    }
    board <- fliplights(sample(1:5, 1), sample(1:5, 1), board)
  }
}

#game loop
while((any(board == 1) || options[2] == 1)) {

    plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(1, 7), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
  if(floor(click$x) > 0 && floor(click$x) < 6 && floor(click$y) > 1 && floor(click$y) < 7 && menu == 1){
    board <- fliplights(floor(click$x), floor(click$y)-1, board)
  }

  
  for(i in 1:5)
    for(j in 1:5)
      rect(i, j + 2 , i + 1, j + 1, col = ifelse(board[j,i]==0, themeColors[[options[1]+1]][1], themeColors[[options[1]+1]][2]), border = "black")
  
  menu <- 1
  if((any(board == 1) || options[2] == 1))
    click <- locator(1)
}

rect(2.5,3.5,4.5,5.5, col="thistle4")
text(3.5,4.5, "You win!")