#install.packages(c("beepr"))
#library(beepr)

#TODO: sounds? disable moving if it does not score points? check if state has no legal moves?

gems <- matrix(0, nrow = 8, ncol = 8)
selected <- rep(-1, times = 4)
menu <- 0
textboxes <- c(0,0)
colorcombo <- c(-1,-1)
gemColors <- c("lightgray", "red", "yellow", "chartreuse", "orange", "cyan", "magenta")

resetboard <- function() {
  for (i in 1:8) {
    gems[i, ] = sample(1:7, 8, replace=TRUE)
  }
  #beep()
  return(gems)
}

checkgems <- function() {
  checkedgems <- list()
  for (i in 1:8) { #vertical
    colorcombo <- c(-1,-1)
    for (j in 1:8) {
      if(colorcombo[1] == -1) {
        colorcombo[1] = gems[i,j]
        colorcombo[2] = 0
      }
      if(colorcombo[1] == gems[i,j]) {
        colorcombo[2] = colorcombo[2] +1
      } else {
        colorcombo[1] = gems[i,j]
        colorcombo[2] = 1
      }
      if(colorcombo[2] == 3) {
        checkedgems <- c(checkedgems, list(list(i,j,"v")))
      }
    }
  }
  
  for (j in 1:8) { #horizontal
    colorcombo <- c(-1,-1)
    for (i in 1:8) {
      if(colorcombo[1] == -1) {
        colorcombo[1] = gems[i,j]
        colorcombo[2] = 0
      }
      if(colorcombo[1] == gems[i,j]) {
        colorcombo[2] = colorcombo[2] + 1
      } else {
        colorcombo[1] = gems[i,j]
        colorcombo[2] = 1
      }
      if(colorcombo[2] == 3) {
        checkedgems <- c(checkedgems, list(list(i,j,"h")))
      }
    }
  }
  return(checkedgems)
}

cleargems <- function(checkedgems) {
  tempgem <- gems
	cleared <- 0
	marked <- matrix(0, nrow = 8, ncol = 8)
	oldmark <- marked
	if(length(checkedgems) > 0) {
  	for(i in 1:length(checkedgems)){
  	  marked[checkedgems[[i]][[1]], checkedgems[[i]][[2]]] = 1
  	}
	}
	
	while(sum(marked) != 0 && sum(oldmark) != sum(marked)) {
		oldmark <- marked
		marked <- markpass(marked)
	}
	
	for (i in 1:8) {
	  for (j in 1:8) {
	    if(marked[i,j] == 1) {
	      tempgem[i,j] = -1 #clear it
	    }
	  }
	}
	cleared <- sum(marked)
	gems <- refill(tempgem)
	return(c(gems,cleared))
}

markpass <- function(marked) {
	for (i in 1:8) {
		for (j in 1:8) {
			if(marked[i,j] == 1) {
				if(i!=1 && (gems[i-1,j] == gems[i,j]) && marked[i-1,j]==0) {
				  marked[i-1, j] = 1
				}
			  if(i!=8 && (gems[i+1,j] == gems[i,j]) && marked[i+1,j]==0) {
			    marked[i+1, j] = 1
			  }
			  if(j!=1 && (gems[i,j-1] == gems[i,j]) && marked[i,j-1]==0) {
			    marked[i, j-1] = 1
			  }
			  if(j!=8 && (gems[i,j+1] == gems[i,j]) && marked[i,j+1]==0) {
			    marked[i, j+1] = 1
			  }
			}
		}
	}
	return(marked)
}

swapgems <- function(row1, col1, row2, col2) {
  temp <- gems[row1, col1]
  gems[row1, col1] = gems[row2, col2]
  gems[row2, col2] = temp
  return(gems)
}

refill <- function(ge) {
  for (z in 1:8) { #fall down 8 times
    for (i in 1:8) {
      for (j in 1:8) {
        if(ge[i, j] == -1) {
          if(j==8) {
            ge[i,j] = sample(1:7, 1)
          } else {
            ge[i,j] = ge[i,j+1]
            ge[i,j+1] = -1
          }
        }
      }
    }
  }
  return(ge)
}

isadjacent <- function(row1, col1, row2, col2) {
  return(((abs(row1-row2) == 1) || (abs(col1-col2) == 1)) && !(((abs(row1-row2) == 1) && (abs(col1-col2) == 1))))
}

trinum <- function(n) {
  return(n * (n + 1) / 2)
}

#board reset and cleanup
gems <- resetboard()
checked <- checkgems()
while(length(checked)>0) {
  clear <- cleargems(checked)
  gems <- matrix(clear[1:64], nrow = 8, ncol = 8)
  checked <- checkgems()
}

#menu
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
par(bg = "darkblue")
text(5,5, "Gem Game (for R)")

while(TRUE) {
click_coordinates <- locator(1)

if(floor(click_coordinates$x) == 0 && floor(click_coordinates$y) == 0) {
  textboxes[2] = textboxes[2] + 1
  gems <- resetboard()
  while(length(checked)>0) {
    clear <- cleargems(checked)
    gems <- matrix(clear[1:64], nrow = 8, ncol = 8)
    checked <- checkgems()
  }
}

#selection
if(selected[1] == -1 && menu != 0) { #pair 1
  if(floor(click_coordinates$x) > 0 && floor(click_coordinates$x) < 9) {
    selected[1] = floor(click_coordinates$x)
  }
  if(floor(click_coordinates$y) > 0 && floor(click_coordinates$y) < 9) {
    selected[2] = floor(click_coordinates$y)
  }
} else { #pair 2
  if(floor(click_coordinates$x) > 0 && floor(click_coordinates$x) < 9) {
    selected[3] = floor(click_coordinates$x)
  }
  if(floor(click_coordinates$y) > 0 && floor(click_coordinates$y) < 9) {
    selected[4] = floor(click_coordinates$y)
  }
}

if(selected[1] != -1 && selected[1] == selected[3] && selected[2] == selected[4]) {
  selected <- rep(-1, times = 4) #deselect
} else if (selected[3] != -1) {
  if(isadjacent(selected[1], selected[2], selected[3], selected[4])) {
    gems <- swapgems(selected[1], selected[2], selected[3], selected[4])
  }
  checked <- checkgems()
  while(length(checked)>0) {
    checked <- checkgems()
    clear <- cleargems(checked)
    gems <- matrix(clear[1:64], nrow = 8, ncol = 8)
    textboxes[1] = textboxes[1] + trinum(clear[65])
  }
  selected <- rep(-1, times = 4)
}

#draw board
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = paste("Score:",textboxes[1]), ylab = paste("Resets:",textboxes[2]), axes = FALSE, frame.plot = FALSE)

for (i in 1:8) {
  for (j in 1:8) {
    rect(i, j, i + 1, j + 1, col = gemColors[gems[i, j]], border = "black")
  }
}

rect(0, 0, 1, 1, col = "purple", border = "white")
text(0.5,0.5, "Rst")
if(selected[1] != -1) {
  rect(selected[1]-0.05, selected[2]-0.05, selected[1] + 1.05, selected[2] + 1.05, col = gemColors[gems[selected[1], selected[2]]], border = "blue")
}

menu <- 1

}
