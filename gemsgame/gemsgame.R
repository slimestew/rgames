par(bg = "darkblue")
gems <- matrix(0, nrow = 8, ncol = 8)
tempgems <- matrix(0, nrow = 8, ncol = 8)
tempcheck <- list()
selected <- rep(-1, times = 4)
menu <- 0
click <- list(x = 3, y = 3)
textboxes <- c(0,0)
colorcombo <- c(-1,-1)
options <- c(FALSE, FALSE, 0)
# Freeswap: can swap as long as spots are adjacent, doesn't need to cause a valid solve
# Resetpt: Subtracts 10 points if resetting freely (does not subtract if a reset is forced)
# Twist: instead of swapping two adjacent gems, rotates a whole 2x2 area (selecting and deselecting inverts)
gemColors <- c("lightgray", "red", "yellow", "chartreuse", "orange", "cyan", "magenta")
btnColors <- c("darkred","green", "yellow")

resetboard <- function() {
  for (i in 1:8)
    gems[i, ] <- sample(1:7, 8, replace=TRUE)
  return(gems)
}

failboard <- function() {
  k <- 0
  for (i in 1:8)
    for (j in 1:8) {
      gems[i, j] <- (k + 1)
      k <- (k+1) %% 7
    }
  return(gems)
}

checkgems <- function(goms) {
  checkedgems <- list()
  for (i in 1:8) { #vertical
    colorcombo <- c(-1,-1)
    for (j in 1:8) {
      if(colorcombo[1] == -1) {
        colorcombo[1] <- goms[i,j]
        colorcombo[2] <- 0
      }
      if(colorcombo[1] == goms[i,j]) {
        colorcombo[2] <- colorcombo[2] +1
      } else {
        colorcombo[1] <- goms[i,j]
        colorcombo[2] <- 1
      }
      if(colorcombo[2] == 3)
        checkedgems <- c(checkedgems, list(list(i,j,"v")))
    }
  }
  
  for (j in 1:8) { #horizontal
    colorcombo <- c(-1,-1)
    for (i in 1:8) {
      if(colorcombo[1] == -1) {
        colorcombo[1] <- goms[i,j]
        colorcombo[2] <- 0
      }
      if(colorcombo[1] == goms[i,j]) {
        colorcombo[2] <- colorcombo[2] + 1
      } else {
        colorcombo[1] <- goms[i,j]
        colorcombo[2] <- 1
      }
      if(colorcombo[2] == 3)
        checkedgems <- c(checkedgems, list(list(i,j,"h")))
    }
  }
  return(checkedgems)
}

legalmoves <- function(goms) {
  if(length(checked)>0)
    return(TRUE) #should never happen, but would be accurate
  
  tempgems <- gems
  #marked function not useful since it would count an L-shaped triomino as valid
  if(options[3]){
    for (i in 1:7) {
      for (j in 1:7) {
        if(options[3]==1){ #clockwise
          tempgems <- twistgems(tempgems, i, j)
          tempcheck <- checkgems(tempgems)
          if(length(tempcheck)>0){
            return(TRUE)
          }
          tempgems <- untwistgems(tempgems, i, j)
        } else { #counterclockwise
          tempgems <- untwistgems(tempgems, i, j)
          tempcheck <- checkgems(tempgems)
          if(length(tempcheck)>0){
            return(TRUE)
          }
          tempgems <- twistgems(tempgems, i, j)
        }
      }
    }
    return(FALSE)
  } else {
    for (i in 1:8) { #vertical
      for (j in 1:7) {
        tempgems <- swapgems(tempgems, i, j, i, j+1)
        tempcheck <- checkgems(tempgems)
        if(length(tempcheck)>0){
          return(TRUE)
        }
        tempgems <- swapgems(tempgems, i, j, i, j+1)
      }
    }
    
    for (j in 1:8) { #horizontal
      for (i in 1:7) {
        swapgems(tempgems, i, j, i+1, j)
        tempcheck <- checkgems(tempgems)
        if(length(tempcheck)>0){
          return(TRUE)
        }
        swapgems(tempgems, i, j, i+1, j)
      }
    }
    return(FALSE)
  }
}

cleargems <- function(checkedgems) {
  tempgem <- gems
	cleared <- 0
	marked <- matrix(0, nrow = 8, ncol = 8)
	oldmark <- marked
	if(length(checkedgems) > 0)
  	for(i in 1:length(checkedgems))
  	  marked[checkedgems[[i]][[1]], checkedgems[[i]][[2]]] <- 1
	
	while(sum(marked) != 0 && sum(oldmark) != sum(marked)) {
		oldmark <- marked
		marked <- markpass(marked)
	}
	
	for (i in 1:8)
	  for (j in 1:8)
	    if(marked[i,j] == 1)
	      tempgem[i,j] <- -1 #clear it
	
	cleared <- sum(marked)
	gems <- refill(tempgem)
	return(c(gems,cleared))
}

markpass <- function(marked) {
	for (i in 1:8) {
		for (j in 1:8) {
			if(marked[i,j] == 1) {
				if(i!=1 && (gems[i-1,j] == gems[i,j]) && marked[i-1,j]==0)
				  marked[i-1, j] <- 1
			  if(i!=8 && (gems[i+1,j] == gems[i,j]) && marked[i+1,j]==0)
			    marked[i+1, j] <- 1
			  if(j!=1 && (gems[i,j-1] == gems[i,j]) && marked[i,j-1]==0)
			    marked[i, j-1] <- 1
			  if(j!=8 && (gems[i,j+1] == gems[i,j]) && marked[i,j+1]==0)
			    marked[i, j+1] <- 1
			}
		}
	}
	return(marked)
}

swapgems <- function(goms, row1, col1, row2, col2) {
  temp <- goms[row1, col1]
  goms[row1, col1] <- gems[row2, col2]
  goms[row2, col2] <- temp
  return(goms)
}

twistgems <- function(goms, row, col) {
  temp <- goms[row, col]
  goms[row, col] <- gems[row+1, col]
  goms[row+1, col] <- gems[row+1, col+1]
  goms[row+1, col+1] <- gems[row, col+1]
  goms[row, col+1] <- temp
  return(goms)
}

untwistgems <- function(goms, row, col) {
  temp <- goms[row, col]
  goms[row, col] <- gems[row, col+1]
  goms[row, col+1] <- gems[row+1, col+1]
  goms[row+1, col+1] <- gems[row+1, col]
  goms[row+1, col] <- temp
  return(goms)
}

refill <- function(ge) {
  for (z in 1:8) { #fall down 8 times
    for (i in 1:8)
      for (j in 1:8)
        if(ge[i, j] == -1)
          if(j==8) {
            ge[i,j] <- sample(1:7, 1)
          } else {
            ge[i,j] <- ge[i,j+1]
            ge[i,j+1] <- -1
          }
  }
  return(ge)
}

isadjacent <- function(row1, col1, row2, col2) {
  return(((abs(row1-row2) <= 1) && (abs(col1-col2) <= 1)) && !(((abs(row1-row2) == 1) && (abs(col1-col2) == 1))))
}

trinum <- function(n) {
  return(n * (n + 1) / 2)
}

#board reset and cleanup
gems <- resetboard()
checked <- checkgems(gems)
while(length(checked)>0) {
  clear <- cleargems(checked)
  gems <- matrix(clear[1:64], nrow = 8, ncol = 8)
  checked <- checkgems(gems)
}

#menu
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
polygon(c(5,1,3,7,9), c(0,4,7,7,4), col="cyan", border="lightblue")
segments(c(1,3,3,7,7), c(4,4.5,4.5,4.5,4.5), c(3,7,5,5,9), c(4.5,4.5,0,0,4), col="lightblue")
par(bg = "darkblue")
text(5,5, "Gem Game (for R)")

while((floor(click$y) < 5 || floor(click$y) > 8) || menu < 2){
  click <- locator(1)
  plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  
  if((floor(click$x) > 1 && floor(click$x) < 7) && menu > 0){
    if(floor(click$y) == 3)
      options[1] <- !options[1]
    if(floor(click$y) == 2)
      options[2] <- !options[2]
    if(floor(click$y) == 1)
      options[3] <- (options[3]+1) %% 3
  }
  
  rect(1, 5, 7, 6, col = "lightblue", border = "cyan")
  rect(1, 3, 7, 4, col = btnColors[options[1]+1], border = "black")
  rect(1, 2, 7, 3, col = btnColors[options[2]+1], border = "black")
  rect(1, 1, 7, 2, col = btnColors[options[3]+1], border = "black")
  text(4,8, "Gem Game")
  text(4,3.5, "Freeswap")
  text(4,2.5, "Resetpts")
  text(4,1.5, ifelse(options[3],ifelse(options[3]==1,"Clockwise","CCW Twist"),"Regular"))
  text(4,5.5, "Start")
  
  menu <- min(menu + 1,2)
}
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
menu <- 0

#game loop
while(TRUE) {

if(floor(click$x) == 0 && floor(click$y) == 0) {
  textboxes[2] <- textboxes[2] + 1
  if(legalmoves(gems) && options[2])
    textboxes[1] <- textboxes[1] - 10

  gems <- resetboard()
  checked <- checkgems(gems)
  while(length(checked)>0) {
    clear <- cleargems(checked)
    gems <- matrix(clear[1:64], nrow = 8, ncol = 8)
    checked <- checkgems(gems)
  }
  
}

#selection
if(options[3]>0) { #twist
  if(selected[1] == -1 && menu != 0) { #pair 1
    if(floor(click$x) > 0 && floor(click$x) < 9)
      selected[1] <- floor(click$x)
    if(floor(click$y) > 0 && floor(click$y) < 9)
      selected[2] <- floor(click$y)
    if(selected[1]==8) selected[1] <- 7
    if(selected[2]==8) selected[2] <- 7
  } else if(floor(click$x) != selected[1] && floor(click$y) != selected[2]) {
    selected[1] <- -1
    selected[2] <- -1
  } else {
    if(!options[1]){
      if(options[3]==1){
        tempgems <- twistgems(gems, selected[1], selected[2])
        tempcheck <- checkgems(tempgems)
        if(length(tempcheck)>0)
          gems <- tempgems
      } else {
        tempgems <- untwistgems(gems, selected[1], selected[2])
        tempcheck <- checkgems(tempgems)
        if(length(tempcheck)>0)
          gems <- tempgems
      }
    } else {
      if(options[3]==1)
        gems <- twistgems(gems, selected[1], selected[2])
      else
        gems <- untwistgems(gems, selected[1], selected[2])
    }
    
    checked <- checkgems(gems)
    while(length(checked)>0) {
      checked <- checkgems(gems)
      clear <- cleargems(checked)
      gems <- matrix(clear[1:64], nrow = 8, ncol = 8)
      textboxes[1] <- textboxes[1] + trinum(clear[65])
    }
    selected <- rep(-1, times = 4)
  }
  
} else { #regular
    if(selected[1] == -1 && menu != 0) { #pair 1
      if(floor(click$x) > 0 && floor(click$x) < 9)
        selected[1] <- floor(click$x)
      if(floor(click$y) > 0 && floor(click$y) < 9)
        selected[2] <- floor(click$y)
    } else { #pair 2
      if(floor(click$x) > 0 && floor(click$x) < 9)
        selected[3] <- floor(click$x)
      if(floor(click$y) > 0 && floor(click$y) < 9)
        selected[4] <- floor(click$y)
    }
  }
  
  if(selected[1] != -1 && selected[1] == selected[3] && selected[2] == selected[4]) {
    selected <- rep(-1, times = 4) #deselect
  } else if (selected[3] != -1 && selected[4] != -1) {
    if(isadjacent(selected[1], selected[2], selected[3], selected[4])) {
      if(!options[1]){
        tempgems <- swapgems(gems, selected[1], selected[2], selected[3], selected[4])
        tempcheck <- checkgems(tempgems)
        if(length(tempcheck)>0)
          gems <- tempgems
      } else {
        gems <- swapgems(gems, selected[1], selected[2], selected[3], selected[4])
      }
    }
    checked <- checkgems(gems)
    while(length(checked)>0) {
      checked <- checkgems(gems)
      clear <- cleargems(checked)
      gems <- matrix(clear[1:64], nrow = 8, ncol = 8)
      textboxes[1] <- textboxes[1] + trinum(clear[65])
    }
    selected <- rep(-1, times = 4)
}

#draw board
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = paste("Score:",textboxes[1]), ylab = paste("Resets:",textboxes[2]), axes = FALSE, frame.plot = FALSE)

for (i in 1:8)
  for (j in 1:8)
    rect(i, j, i + 1, j + 1, col = gemColors[gems[i, j]], border = "black")

rect(0, 0, 1, 1, col = "purple", border = "white")
text(0.5,0.5, "Rst")
if(options[3]) {
  if(selected[1] != -1 && selected[2] != -1)
    rect(selected[1]-0.05, selected[2]-0.05, selected[1] + 2.025, selected[2] + 2.05, col = "transparent" , border = "blue")
} else {
  if(selected[1] != -1 && selected[2] != -1)
    rect(selected[1]-0.05, selected[2]-0.05, selected[1] + 1.05, selected[2] + 1.05, col = gemColors[gems[selected[1], selected[2]]], border = "blue")
}

if(!legalmoves(gems))
  text(5,0.5, "No legal moves! Reset")

menu <- 1

click <- locator(1)
}
