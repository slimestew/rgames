#TODO: make the game work, make the pile draw correctly

par(bg = "navajowhite")
cards <- list() #all cards, shuffles
deck <- list() #draw pile
stack <- list() #play pile
selected <- 0
pause <- FALSE
menu <- 0
win <- TRUE
turnorder <- c(1, 1) #player and direction
click <- list(x = 3, y = 3)
textboxes <- list(0,0)
options <- c(0, 0, 0, 0)
# Variants:
#     Crazy Eights(standard deck, no power cards, score based)
#     Last Card(standard deck, power cards, shedding wins)
#     One(unique deck, power cards, shedding wins)
#     Other One(unique deck, power cards, shedding wins)
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

dispCard <- function(x, y, suit, valu) {
  rect(x,y,x+1,y+2, col="white", border="black")
  if(options[1]<2) #colors/cards
    text(x+0.5,y+1.5, suits[suit], col=suitColors[suit])
  else
    if(options[1]==2){
      rect(x,y+1,x+1,y+2, col=oneColors[suit], border="black")
    } else { 
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

validCard <- function(suit, valu) {
  return(suit == stack[[length(stack)]][[1]][[1]] || valu == stack[[length(stack)]][[1]][[2]] || valu == "C" || valu == "F")
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
      options[1] = (options[1]+1) %% 4
    if(floor(click$y) == 2)
      options[2] = !options[2]
    if(floor(click$y) == 1)
      options[3] = !options[3]
    if(floor(click$y) == 0)
      options[4] = (options[4]+1) %% 6
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
  
  menu = min(menu + 1,2)
}

plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
menu <- 0

cards <- sample(unpack(), replace=TRUE)
deck <- cards
hands <- replicate(options[4]+2, list())
for(i in 1:(options[4]+2)){
if(options[1]<2 && options[4] != 0) {
  for(j in 1:5){
    temp <- sample(length(deck), 1)
    hands[[i]] = c(hands[[i]], list(deck[temp][1]))
    deck <- deck[-temp]
  }
} else {
    for(j in 1:7){
      temp <- sample(length(deck), 1)
      hands[[i]] = c(hands[[i]], list(deck[temp][1]))
      deck <- deck[-temp]
    }
  }
}
temp <- sample(length(deck), 1)
stack = list(deck[temp][1])
deck <- deck[-temp]

#game loop
while(win) {

if(menu>0){
  if(floor(click$x) > 0 && floor(click$x) < 9 && floor(click$y) > -1 && floor(click$y) < 3) #selection
    selected[1] = floor(click$x)
  if(floor(click$x) > 2 && floor(click$x) < 5 && floor(click$y) > 3 && floor(click$y) < 5){ #draw card
    hands[[turnorder[2]]] = c(hands[[turnorder[2]]], list(deck[length(deck)][1]))
    deck <- deck[-length(deck)]
    if(turnorder[1]){ #next player, move this outside the loop when made more playable
      turnorder[2] = (turnorder[2] %% (options[4]+2)) + 1
      pause <-TRUE
    } else {
      turnorder[2] = ((turnorder[2]-2) %% (options[4]+2)) + 1
    }
    selected <- 0
  }
}
  
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = ifelse(options[1]==0,paste("Score:",textboxes),""), ylab = "", axes = FALSE, frame.plot = FALSE)
rect(0,4.5,10,6.5, col="darkorange4", border="black")
rect(0,-2,10,6, col="brown", border="black")

if(!pause){
  
  for(i in floor(length(deck)/8):1) #todo: make this fall properly
    rect(3,4-(i*0.1),5,5-(i*0.1), col="chartreuse4", border="black")
  rect(3,4,5,5, col="chartreuse3", border="black")
  rect(3.25,4.25,4.75,4.75, col="chartreuse4", border="white") 
  
  for(i in 1:(options[4]+1)){
    rect(1+i,8,2+i,10, col="chartreuse3", border="black")
    rect(1.25+i,8.25,1.75+i,9.75, col="chartreuse4", border="white")
    if(i>=turnorder[2]+1)
      text(1.5+i, 7.5, length(hands[[i]]))
    else
      text(1.5+i, 7.5, length(hands[[i+1]]))
  }
  dispCard(5, 4, stack[[length(stack)]][[1]][[1]], stack[[length(stack)]][[1]][[2]])
    
  for(i in 1:length(hands[[turnorder[2]]])) #display cards(how to order? spaced out if few, navbars if many)
    dispCard(i, (selected==i), hands[[turnorder[2]]][[i]][[1]][[1]], hands[[turnorder[2]]][[i]][[1]][[2]])

} else {
  text(4,4, paste("Click for Player",turnorder[2]))
  menu <- -1
}
  
menu <- min(menu+1, 1)

click <- locator(1)

if(pause)
  pause <- FALSE

if(floor(click$y) < 0)
    win = FALSE
}
