#TODO: make the game work, make the pile draw correctly

par(bg = "navajowhite")
cards <- list() #all cards, shuffles
deck <- list() #draw pile
selected <- 2
menu <- 0
win <- TRUE
turnorder <- c(1, 1)
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
uqVals <- c("0","1","2","3","4","5","6","7","8","9","C","S","R","T","F")
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
print(deck[2])
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

#game loop
while(win) {
  
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = ifelse(options[1]==0,paste("Score:",textboxes),""), ylab = "", axes = FALSE, frame.plot = FALSE)
rect(0,4.5,10,6.5, col="darkorange4", border="black")
rect(0,-2,10,6, col="brown", border="black")


for(i in floor(length(deck)/8):1) #todo: make this fall properly
  rect(3,4-(i*0.1),5,5-(i*0.1), col="chartreuse4", border="black")
rect(3,4,5,5, col="chartreuse3", border="black")
rect(3.25,4.25,4.75,4.75, col="chartreuse4", border="white") 

for(i in 1:(options[4]+1)){
  rect(1+i,8,2+i,10, col="chartreuse3", border="black")
  rect(1.25+i,8.25,1.75+i,9.75, col="chartreuse4", border="white")
  text(1.5+i, 7.5, length(hands[[turnorder[2]]]))
}
  
for(i in 1:length(hands[[turnorder[2]]])){ #display cards(how to order? spaced out if few, navbars if many)
  rect(i,(selected==i),1+i,2+(selected==i), col="white", border="black")
  if(options[1]<2) #colors/cards
    text(0.5+i,1.5+(selected==i), suits[hands[[turnorder[2]]][[i]][[1]][[1]]], col=suitColors[hands[[turnorder[2]]][[i]][[1]][[1]]])
  else
    if(options[1]==2){
      rect(i,1+(selected==i),1+i,2+(selected==i), col=oneColors[hands[[turnorder[2]]][[i]][[1]][[1]]], border="black")
    } else { 
      rect(i,1+(selected==i),1+i,2+(selected==i), col=primaryColors[hands[[turnorder[2]]][[i]][[1]][[1]]], border="black")
    }
  
  if(hands[[turnorder[2]]][[i]][[1]][2] == "T"){ #face values
    text(0.5+i,0.5+(selected==i), "+2")
  } else if(hands[[turnorder[2]]][[i]][[1]][2] == "S"){
    text(0.5+i,0.5+(selected==i), "(X)")
  } else if(hands[[turnorder[2]]][[i]][[1]][2] == "R"){
    text(0.5+i,0.5+(selected==i), "<>")
  } else if(hands[[turnorder[2]]][[i]][[1]][2] == "C"){
    rect(i,1+(selected==i),1+i,2+(selected==i), col="gray12", border="black")
    text(0.5+i,0.5+(selected==i), " ")
  } else if(hands[[turnorder[2]]][[i]][[1]][2] == "F"){
    rect(i,1+(selected==i),1+i,2+(selected==i), col="gray12", border="black")
    text(0.5+i,0.5+(selected==i), "+4")
  } else {
    text(0.5+i,0.5+(selected==i), hands[[turnorder[2]]][[i]][[1]][2])
  }
}
  
menu <- 1

click <- locator(1)
win = FALSE
}
