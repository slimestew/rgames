#TODO: make the game work, make the pile draw correctly

par(bg = "navajowhite")
cards <- list() #all cards, shuffles
deck <- list() #draw pile
stack <- list() #play pile
wildcard <- 0 #wildcard
drawCount <- -1 #+2 and +4
selected <- 0
handOffset <- 0
pause <- FALSE
menu <- 0
win <- TRUE
turnorder <- c(1, 1, FALSE) #player, direction, move played
click <- list(x = 3, y = 3)
textboxes <- list(0,0)
options <- c(0, 0, 0, 0)
# Variants:
#     Crazy Eights(standard deck, minimal power cards, score based)
#     Last Card(standard deck, some power cards, shedding wins)
#     One(unique deck, power cards, shedding wins)
#     Other One(unique deck, power cards, can challenge +4, shedding wins)
# Queen Swap: (only for Crazy Eights)
#     Playing a queen causes a player to skip their turn
# 0's and 7's (only for One and Other One)
#     Playing a 7 allows the player to swap hands with someone else
#     Playing a 0 swaps all hands one over by turn order.
# AI or Human opponents
# Number of players
hands <- matrix()
stVals <- c("1","2","3","4","5","6","7","8","9","J","Q","K","A")
uqVals <- c("0","1","2","3","4","5","6","7","8","9","T","S","R","C","F")
suits <- c("♥", "♦", "♠", "♣")
variants <- c("Crazy Eights", "Last Card", "One", "Other One")
suitColors <- c("red", "red", "black", "black")
primaryColors <- c("red","yellow", "limegreen", "blue")
oneColors <- c("#ff0077","#ffaa00", "#448800", "#0066ff")

#functions, reset cards

unpack <- function() {
  for(i in 1:4)
  if(options[1]<2) {
    for(j in 1:13)
      cards <- c(cards, list(list(i,stVals[j])))
    if(options[4]>3)
      for(j in 1:13)
        cards <- c(cards, list(list(i,stVals[j])))
  } else {
    for(j in 1:15)
      cards <- c(cards, list(list(i,uqVals[j])))
    for(j in 2:13)
      cards <- c(cards, list(list(i,uqVals[j])))
  }
  return(cards)
}

dispCard <- function(x, y, card) {
  suit <- card[[1]]
  valu <- card[[2]]
  rect(x,y,x+1,y+2, col="white", border="black")
  if(options[1]<2) { #colors/cards
    text(x+0.5,y+1.5, suits[suit], col=suitColors[suit])
  } else {
    if(options[1]==2)
      rect(x,y+1,x+1,y+2, col=oneColors[suit], border="black")
    else
      rect(x,y+1,x+1,y+2, col=primaryColors[suit], border="black")
  }
  
  if(valu == "T"){ #face values
    text(0.5+x,0.5+y, "+2")
  } else if(valu == "S"){
    text(0.5+x,0.5+y, "(X)")
  } else if(valu == "R"){
    text(0.5+x,0.5+y, "<>")
  } else if(valu == "C"){
    rect(x,y+1,x+1,y+2, col="gray12", border="black")
    text(0.5+x,0.5+y, " ")
  } else if(valu == "F"){
    rect(x,y+1,x+1,y+2, col="gray12", border="black")
    text(0.5+x,0.5+y, "+4")
  } else {
    text(0.5+x,0.5+y, valu)
  }
}

wildColor <- function() {
  rect(0,0,10,3, col="#a52a2a80", border="transparent")
  while(TRUE){
    dispCard(1, 2, list(1, " "))
    dispCard(3, 2, list(2, " "))
    dispCard(5, 2, list(3, " "))
    dispCard(7, 2, list(4, " "))
    click <- locator(1)
    if(floor(click$x) < 2)
      return(1)
    if(floor(click$x) < 4)
      return(2)
    if(floor(click$x) < 6)
      return(3)
    if(floor(click$x) < 8)
      return(4)
  }
}

validCard <- function(card) {
  suit <- card[[1]]
  valu <- card[[2]]
  return( (suit == stack[[length(stack)]][[1]] && !wildcard && drawCount<0) || valu == stack[[length(stack)]][[2]] || valu == "C" ||
          valu == "F" || (options[1]==0 && valu== "8") || (options[1]==1 && valu== "A") || (options[1]==1 && valu== "J") || (wildcard && suit == wildcard))
}

nextPlayer <- function() {
  if(turnorder[1])
    return((turnorder[2] %% (options[4]+2)) + 1)
  return(((turnorder[2]-2) %% (options[4]+2)) + 1)
}

doAI <- function(deck, hands, stack, wildcard, drawCount) {
  turnOver <- FALSE
  while(turnorder[2]!=1){
    for(i in 1:length(hands[[turnorder[2]]])){ #play
      if(validCard(hands[[turnorder[2]]][[i]])){
        stack <- c(stack, list(hands[[turnorder[2]]][[i]]))
        hands[[turnorder[2]]] <- hands[[turnorder[2]]][-(i)]
        wildcard <- 0
        turnOver <- TRUE
        break
      }
      #TODO: power cards
    }
      if(!turnOver){ #draw
        hand <- max(1, drawCount)
        for(i in 1:hand){
          hands[[turnorder[2]]] <- c(hands[[turnorder[2]]], deck[length(deck)][1])
          deck <- deck[-length(deck)]
        }
        drawCount <- -1
      }
    turnorder[2] <- nextPlayer() 
  }
  return(list(deck, hands, stack, wildcard, drawCount))
}

#menu
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), col="white", xlab = "by slimestew", ylab = "", axes = FALSE, frame.plot = TRUE)
par(bg = "navajowhite")
text(5,5, "Crazy Eights (for R)")

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
  text(4,1.5, ifelse(options[3],"AI","Human"))
  text(4,0.5, options[4]+2)
  text(4,5.5, "Start")
  
  menu <- min(menu + 1,2)
}

plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
menu <- 0

cards <- sample(unpack(), replace=TRUE)
deck <- cards
hands <- replicate(options[4]+2, list())
for(i in 1:(options[4]+2)){
  hand<-7
  if(options[1]<2 && options[4] != 0)
    hand<-5
  
  for(j in 1:hand){
    temp <- sample(length(deck), 1) #don't need to sample here, already randomized
    hands[[i]] <- c(hands[[i]], deck[temp][1])
    deck <- deck[-temp]
  }
}
temp <- sample(length(deck), 1) #don't need to sample here, already randomized
stack <- deck[temp][1]
deck <- deck[-temp]

# add card
# hands[[1]] <- c(hands[[1]], list(list(list(1,"2"))))
# hands[[1]] <- c(hands[[1]], list(list(list(2,"2"))))
if(stack[[length(stack)]][[2]] == "C" || stack[[length(stack)]][[2]] == "T" || stack[[length(stack)]][[2]] == "F"){
  stack <- c(stack, deck[length(deck)][1])
  deck <- deck[-length(deck)]
}

#game loop
while(win) {

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
          turnorder[2] <- nextPlayer()
      } else if(options[1] == 1) { #last card
        if(stack[[length(stack)]][[2]] == "8")
          turnorder[2] <- nextPlayer()
        if(stack[[length(stack)]][[2]] == "J" || stack[[length(stack)]][[2]] == "A")
          wildcard <- wildColor()
        if(drawCount == -1 && (stack[[length(stack)]][[2]] == "2" || stack[[length(stack)]][[2]] == "A"))
          drawCount <- 0
      } else if(options[1] >= 2){ #one and other one
        if(stack[[length(stack)]][[2]] == "S")
          turnorder[2] <- nextPlayer()
        if(stack[[length(stack)]][[2]] == "R")
          turnorder[1] <- !turnorder[1]
        if(stack[[length(stack)]][[2]] == "C" || stack[[length(stack)]][[2]] == "F")
          wildcard <- wildColor()
        if(drawCount == -1 && (stack[[length(stack)]][[2]] == "T" || stack[[length(stack)]][[2]] == "F"))
          drawCount <- 0
      }
      turnorder[3] <- TRUE
      selected <- -1
      handOffset <- 0
    } else {
      selected <- 0
    }
  }
    
  if(floor(click$x) > 2 && floor(click$x) < 5 && click$y > 3.5 && click$y <= 5){ #draw card
    if(length(deck) == 0){
      #TODO: get everything but last card, shuffle deck
      if(options[1]==0){
        #TODO: if crazy eights, give score to lowest player
      }
    }
    hand <- max(1, drawCount)
    for(i in 1:hand){
      hands[[turnorder[2]]] <- c(hands[[turnorder[2]]], deck[length(deck)][1])
      deck <- deck[-length(deck)]
    }
    selected <- -1
    drawCount <- -1
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
    turnorder[2] <- nextPlayer()
    selected <- 0
  }
  
  if(options[3] && turnorder[2]!=1){
    AI <- doAI(deck, hands, stack, wildcard, drawCount)
    deck <- AI[[1]]
    hands <- AI[[2]]
    stack <- AI[[3]]
    wildcard <- AI[[4]]
    drawCount <- AI[[5]]
    if(any(sapply(hands, length) == 0))
      win <- TRUE
    pause <- TRUE
    turnorder[2] <- 1
  }
}
  
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = ifelse(options[1]==0,paste("Score:",textboxes),""), ylab = "", axes = FALSE, frame.plot = FALSE)
rect(0,4.5,10,6.5, col="darkorange4", border="black")
rect(0,-2,10,6, col="brown", border="black")

if(!pause){
  
  for(i in floor(length(deck)/8):1) #TODO: make this fall properly
    rect(3,4-(i*0.1),5,5-(i*0.1), col="chartreuse4", border="black")
  rect(3,4,5,5, col="chartreuse3", border="black")
  rect(3.25,4.25,4.75,4.75, col="chartreuse4", border="white") 
  
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
  dispCard(5, 4, stack[[length(stack)]])
    
  if(win){
    for(i in (1+handOffset):min(length(hands[[turnorder[2]]]), handOffset+8))
      dispCard(i-handOffset, ((selected+handOffset)==i), hands[[turnorder[2]]][[i]])
    
    if(options[1]==0 && any(textboxes >= ((2+options[4])*50))){
      text(4,4, paste("Tallying points",turnorder[2]))
      win <- FALSE
      click <- locator(1)
    }
  }

} else {
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
}