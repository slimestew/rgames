par(bg = "lightsalmon")
plot(0, 0, type = "n", xlim = c(1, 10), ylim = c(1, 10), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
pause <- FALSE
menu <- 0
win <- TRUE
click <- list(x = 3, y = 3)
options <- c(0, 0, 0, 0)

drawDpad <- function(hands, player){
  rect(0,0,10,3, col="#a52a2a80", border="transparent")
  #up down left right button(pickup/drop like in adventure)
    return(1)
  
}

#menu
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
par(bg = "navajowhite")
for(i in 1:4){
  rect(i*2-1,6,i*2,8, col="white", border="black")
  text(i*2-0.5,7.5, suits[i], col=suitColors[i])
  text(i*2-0.5,6.5, "8")
}
text(4.5,5, "Crazy Eights (for R)")

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
  
  rect(1, 5, 7, 6, col = "tan", border = "brown")
  rect(1, 3, 7, 4, col = primaryColors[options[1]+1], border = "black")
  rect(1, 2, 7, 3, col = primaryColors[options[2]+1], border = "black")
  rect(1, 1, 7, 2, col = primaryColors[options[3]+1], border = "black")
  rect(1, 0, 7, 1, col = "white", border = "black")
  text(4,8, "Crazy Eights")
  text(4,3.5, variants[options[1]+1])
  text(4,2.5, ifelse(options[1]<2, ifelse(options[1]," ","Queen Skip"),"0s and 7s"))
  text(4,1.5, ifelse(options[3],"CPU","Human"))
  text(4,0.5, options[4]+2)
  text(4,5.5, "Start")
  
  menu <- min(menu + 1,2)
}

plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
menu <- 0

cards <- sample(unpack(), replace=TRUE)
deck <- cards
hands <- replicate(options[4]+2, list())
scores <- rep(0, options[4]+2)
for(i in 1:(options[4]+2)){
  hand<-7
  if(options[1]<2 && options[4] != 0)
    hand<-5
  
  for(j in 1:hand){
    temp <- sample(length(deck), 1)
    hands[[i]] <- c(hands[[i]], deck[temp][1])
    deck <- deck[-temp]
    if(j == 2)
      next
  }
}
temp <- sample(length(deck), 1)
stack <- deck[temp][1]
deck <- deck[-temp]

maxScore <- (options[4]+2) * 50

# add card
# hands[[1]] <- c(hands[[1]], list(list(2,"0")))

if(stack[[length(stack)]][[2]] == "C" || stack[[length(stack)]][[2]] == "T" || stack[[length(stack)]][[2]] == "F"){
  stack <- c(stack, deck[length(deck)][1])
  deck <- deck[-length(deck)]
}

#game loop
while(win && (options[1]==0) == all(scores < maxScore)) {

if(menu>0){
  
  if(floor(click$x) > 0 && floor(click$x) < 9 && floor(click$y) > -1 && floor(click$y) < 3) #selection
    selected <- floor(click$x)
  
  if(floor(click$x) > 8 && floor(click$y) > -1 && floor(click$y) < 3 && length(hands[[turnorder[2]]]) > (handOffset+8)){
    handOffset <- handOffset+8
    selected <- 0
  }
  
  if(floor(click$x) < 1 && floor(click$y) > -1 && floor(click$y) < 3 && handOffset > 0){
    handOffset <- handOffset-8
    selected <- 0
  }
  
  if(floor(click$x) > 4 && floor(click$x) < 6 && floor(click$y) > 3 && floor(click$y) < 6 && selected[1]){ #play card
    if(validCard(hands[[turnorder[2]]][[selected+handOffset]])){
      stack <- c(stack, list(hands[[turnorder[2]]][[selected+handOffset]]))
      hands[[turnorder[2]]] <- hands[[turnorder[2]]][-(selected+handOffset)]
      
      if(wildcard)
        wildcard <- 0
      
      if(options[1] == 0){  #crazy eights
        if(stack[[length(stack)]][[2]] == "8")
          wildcard <- wildColor()
        if(stack[[length(stack)]][[2]] == "Q" && options[2])
          turnorder[2] <- nextPlayer(turnorder, options)
      } else if(options[1] == 1) { #last card
        if(stack[[length(stack)]][[2]] == "8")
          turnorder[2] <- nextPlayer(turnorder, options)
        if(stack[[length(stack)]][[2]] == "J" || stack[[length(stack)]][[2]] == "A")
          wildcard <- wildColor()
        if(drawCount == -1 && (stack[[length(stack)]][[2]] == "2" || stack[[length(stack)]][[2]] == "A"))
          drawCount <- 0
      } else if(options[1] >= 2){ #one and other one
        if(stack[[length(stack)]][[2]] == "S")
          turnorder[2] <- nextPlayer(turnorder, options)
        if(stack[[length(stack)]][[2]] == "R")
          turnorder[1] <- !turnorder[1]
        if(stack[[length(stack)]][[2]] == "C" || stack[[length(stack)]][[2]] == "F")
          wildcard <- wildColor()
        if(drawCount == -1 && (stack[[length(stack)]][[2]] == "T" || stack[[length(stack)]][[2]] == "F"))
          drawCount <- 0
        if(stack[[length(stack)]][[2]] == "0" && options[2])
          hands <- rotateHands(hands, options)
        if(stack[[length(stack)]][[2]] == "7" && options[2])
          hands <- swapUI(hands, turnorder[2])
      }
      turnorder[3] <- TRUE
      selected <- -1
      handOffset <- 0
    } else {
      selected <- 0
    }
  }
    
  if(floor(click$x) > 2 && floor(click$x) < 5 && click$y > 3.5 && click$y <= 5){ #draw card
    if(length(deck) == 0 && length(stack)>1){
      tempcard <- stack[length(stack)]
      deck <- sample(stack[1:(length(stack)-1)], replace = TRUE)
      if(options[1]==0){
        sums <- c()
        for(i in 1:(options[4]+2)){ #total scores
          sums[i] <- 0
          for(j in 1:length(hands[[i]])){
            if(hands[[i]][[j]][2] == "8")
              sums[i] <- sums[[i]] + 50
            else if(hands[[i]][[j]][2] == "J")
              sums[i] <- sums[[i]] + 10
            else if(hands[[i]][[j]][2] == "Q")
              sums[i] <- sums[[i]] + 10
            else if(hands[[i]][[j]][2] == "K")
              sums[i] <- sums[[i]] + 10
            else if(hands[[i]][[j]][2] == "X")
              sums[i] <- sums[[i]] + 10
            else if(hands[[i]][[j]][2] == "A")
              sums[i] <- sums[[i]] + 1
            else
              sums[i] <- sums[[i]] + as.integer(hands[[i]][[j]][[2]])
          }
        }
        for(i in 1:(options[4]+2)){
          if(sums[i] == min(sums))
            scores[i] <- scores[i] + floor((max(sums) - min(sums)) / sum(sums == min(sums))) #difference of points divided by number of lowest values (on tie)
        }
        
      }
      stack <- tempcard
    }
    if(length(deck)>0){
      hand <- max(1, drawCount)
      for(i in 1:hand){
        hands[[turnorder[2]]] <- c(hands[[turnorder[2]]], deck[length(deck)][1])
        deck <- deck[-length(deck)]
      }
      if(!turnorder[3]){
        turnorder[2] <- nextPlayer(turnorder, options)
        pause <-TRUE
      }
      selected <- -1
      handOffset <- 0
      drawCount <- -1
    }
  }
  
  if(length(hands[[turnorder[2]]]) == 0)
    win <-FALSE
  
  if(selected == -1 && win && turnorder[3]){  #drawing cards
      if(options[1] == 1){
        if(stack[[length(stack)]][[2]] == "2" && drawCount >= 0)
          drawCount <- drawCount+2
        if(stack[[length(stack)]][[2]] == "A" && drawCount >= 0)
          drawCount <- drawCount+4
      } else if(options[1] >= 2){
        if(stack[[length(stack)]][[2]] == "T" && drawCount >= 0)
          drawCount <- drawCount+2
        if(stack[[length(stack)]][[2]] == "F" && drawCount >= 0)
          drawCount <- drawCount+4
      }
    
    if(!options[3])
      pause <-TRUE
    turnorder[2] <- nextPlayer(turnorder, options)
    selected <- 0
  }
  
  if(options[3] && turnorder[2]!=1){
    AI <- doAI(deck, hands, stack, wildcard, drawCount, options)
    deck <- AI[[1]]
    hands <- AI[[2]]
    stack <- AI[[3]]
    wildcard <- AI[[4]]
    drawCount <- AI[[5]]
    if(any(sapply(hands, length) == 0)){
      win <- FALSE
    } else {
      turnorder[2] <- 1
    }
    pause <- TRUE
  }
}
  
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = ifelse(options[1]==0,paste("Score:",scores[turnorder[2]]),""), ylab = "", axes = FALSE, frame.plot = FALSE)
rect(0,4.5,10,6.5, col="darkorange4", border="black")
rect(0,-2,10,6, col="brown", border="black")

if(!pause){
  
  if(length(deck) == 0){
    rect(3,4,5,5, col="brown", border="black", lty = "dashed")
  } else {
    i <- 0
    if(floor(length(deck)/8) > 0)
      for(i in 1:floor(length(deck)/8)) #TODO: make this fall properly
        rect(3,3.5+(i*0.1),5,4.5+(i*0.1), col="chartreuse4", border="black")
    i <- i+1
    rect(3,3.5+(i*0.1),5,4.5+(i*0.1), col="chartreuse3", border="black")
    rect(3.25,3.75+(i*0.1),4.75,4.25+(i*0.1), col="chartreuse4", border="white")
  }
  
  for(i in 1:(options[4]+2)){
    if(i != turnorder[2]){
    rect((i < turnorder[2])+i,8,i+1+(i < turnorder[2]),10, col="chartreuse3", border="black")
    rect(i+0.25+(i < turnorder[2]),8.25,i+0.75+(i < turnorder[2]),9.75, col="chartreuse4", border="white")
    text(i+0.5+(i < turnorder[2]), 7.5, length(hands[[i]]))
    }
  }
  
  text(0.5, 3, length(hands[[turnorder[2]]]), col="tan") 
  
  if(length(hands[[turnorder[2]]]) > handOffset+8){
    symbols(9.5, 1, circles = 0.5, inches = FALSE, add = TRUE, bg = "white", fg="brown")
    text(9.5, 1, ">", col="black") 
  }
  
  if(handOffset > 0){
    symbols(0.5, 1, circles = 0.5, inches = FALSE, add = TRUE, bg = "white", fg="brown")
    text(0.5, 1, "<", col="black")
  }
    
  
  if(drawCount>0){ # +2 and +4s
      symbols(1.5, 5, circles = 0.5, inches = FALSE, add = TRUE, bg = "black", fg="brown")
      text(1.5,5, paste("+",abs(drawCount), sep=""), col="white")
  }
  
  if(wildcard){
    if(options[1]<2){ #colors/cards
      symbols(6.5, 5, circles = 0.5, inches = FALSE, add = TRUE, bg = "white", fg="brown")
      text(6.5,5, suits[wildcard], col=suitColors[wildcard])
    } else {
      if(options[1]==2){
        symbols(6.5, 5, circles = 0.5, inches = FALSE, add = TRUE, bg = oneColors[wildcard], fg="brown")
      } else {
        symbols(6.5, 5, circles = 0.5, inches = FALSE, add = TRUE, bg = primaryColors[wildcard], fg="brown")
      }
    }
  }
  
  text(1,-1, paste("Player",turnorder[2]), xpd=TRUE, col="darkorange4")
  
  dispCard(5, 4, stack[[length(stack)]])
    
  if(win){
    for(i in (1+handOffset):min(length(hands[[turnorder[2]]]), handOffset+8))
      dispCard(i-handOffset, ((selected+handOffset)==i), hands[[turnorder[2]]][[i]])
  } else if(options[1]==0 && any(scores <= maxScore)){
    text(4,2, paste("Tallying points"))
    
    for(i in 1:(options[4]+2)){
      sum <- 0
      if(i==turnorder[2])
        next
      for(j in 1:length(hands[[i]])){
          if(hands[[i]][[j]][2] == "8")
            hands[[i]][[j]][2] <- 50
          else if(hands[[i]][[j]][2] == "J")
            hands[[i]][[j]][2] <- 10
          else if(hands[[i]][[j]][2] == "Q")
            hands[[i]][[j]][2] <- 10
          else if(hands[[i]][[j]][2] == "K")
            hands[[i]][[j]][2] <- 10
          else if(hands[[i]][[j]][2] == "X")
            hands[[i]][[j]][2] <- 10
          else if(hands[[i]][[j]][2] == "A")
            hands[[i]][[j]][2] <- 1
          else
            hands[[i]][[j]][2] <- as.integer(hands[[i]][[j]][[2]])
          sum <- sum + hands[[i]][[j]][[2]]
      }
      scores[turnorder[2]] <- scores[turnorder[2]] + sum
    }
      
    if(all(scores < maxScore) && options[1] == 0)
      win <- TRUE
    
    deck <- cards
    hands <- replicate(options[4]+2, list())
    for(i in 1:(options[4]+2)){
      hand<-7
      if(options[1]<2 && options[4] != 0)
        hand<-5
      
      for(j in 1:hand){
        temp <- sample(length(deck), 1)
        hands[[i]] <- c(hands[[i]], deck[temp][1])
        deck <- deck[-temp]
        if(j == 2)
          next
      }
    }
    temp <- sample(length(deck), 1)
    stack <- deck[temp][1]
    deck <- deck[-temp]
    turnorder[2] <- 1
    selected <- 0
    handOffset <- 0
  }

} else {
  if(win)
    text(4,4, paste("Click for Player",turnorder[2]))
  menu <- -1
}

menu <- min(menu+1, 1)

click <- locator(1)

if(pause)
  pause <- FALSE
}

if(!options[3] || (length(hands[[1]]) == 0 && options[3])){
  text(5,3, "You win!")
} else {
  text(5,3, "Game Over")
  text(5,2, paste0("Player ", which.min(sapply(my_list, length)), " wins"))
}