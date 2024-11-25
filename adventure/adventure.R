par(bg = "white")
plot(0, 0, type = "n", xlim = c(1, 10), ylim = c(1, 10), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
pause <- FALSE
menu <- 0
win <- TRUE
click <- list(x = 3, y = 3)
options <- c(0, 0, 0, 0)

drawDpad <- function(){
  rect(-10,-10,10,10, col="#ffffff80", border="transparent")
  pos <- 5
  rect(4,2,6,8, col = "#80808080", border="black")
  rect(2,4,8,6, col = "#80808080", border="black")
  click <- locator(1)
  if(floor(click$x) < 4) {
    pos <- 2
  } else if(floor(click$x) < 6) {
    if(floor(click$y) > 6) {
      pos <- 1
    } else if(floor(click$y) < 4) {
      pos <- 5
    } else {
      pos <- 3
    }
  } else {
    pos <- 4
  }
    return(c(pos,click))
}

#menu
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
par(bg = "white")
text(4.5,5, "Adventure")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- (options[1]+1) %% 4
    if(floor(click$y) == 2)
      options[2] <- !options[2]
    if(floor(click$y) == 1)
      options[3] <- !options[3]
    if(floor(click$y) == 0)
      options[4] <- (options[4]+1) %% 6
  }
  
  rect(1, 5, 7, 6, col = "white", border = "black")
  rect(1, 3, 7, 4, col = "white", border = "black")
  text(4,8, "Adventure")
  text(4,3.5, "placeholder")
  text(4,5.5, "Start")
  
  menu <- min(menu + 1,2)
}

plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
menu <- 0

#game loop
while(win) {
  
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)

click <- drawDpad()

if(floor(click$y) < 1)
  win <- FALSE
  
menu <- min(menu+1, 1)

if(pause)
  pause <- FALSE
}