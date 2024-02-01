# editR
#
filename <- ""
quit <- FALSE
lines <- 12
scroll <- 0
par(bg = "white")
plot(0, 0, type = "n", xlim = c(0, lines), ylim = c(0, lines), col="white", family = "monospace", main = "", xlab = "", ylab = "H for help", axes = FALSE, frame.plot = FALSE)
size <- dev.size()
justify <- 0
linespacing <- size[2]/4.632



for(i in 1:lines)
  displayLines[i] <- paste(rep("xyzy", i), collapse="")
user_input <- ""

while(!quit){
  
dispFilename <- filename
if(dispFilename=="")
  dispFilename <- "editR"

plot(0, 0, type = "n", xlim = c(0, lines), ylim = c(0, lines), col="white", family = "monospace", main = "", xlab = "", ylab = "H for help", axes = FALSE, frame.plot = FALSE)
rect(-10, -10, 20, 20, col = "#aa77dd")
rect(0,13,1,11.8, col="red", border="transparent")
legend(-1.5, 13+(scroll*linespacing), title=dispFilename, bty="n", displayLines, inset = c(0,0), xjust=justify, y.intersp=linespacing, xpd=TRUE, trace=TRUE)

user_input <- readline("> ")

if(user_input == "H")
  displayLines <- c("D: Inserts line","A: Inserts a line At a position", "UIOP: Moves cursor Left/Down/Up/Right", "H: writes out this help text","J: Adjusts Left-justify strength (D sets to default)","L: Sets line count","S: Saves to file","P: Adjusts Line spacing","99999999999999999999","10","11","12","13","14","15","Q: Quits")
if(user_input == "J"){
  user_input <- readline("> ")
  if(!is.na(as.numeric(user_input)))
     justify <- as.numeric(user_input)
  if(user_input == "D")
    justify <- 0
}
if(user_input == "P"){
  user_input <- readline("> ")
  if(!is.na(as.numeric(user_input)))
    linespacing <- as.numeric(user_input)
  if(user_input == "D")
    linespacing <- size[2]*0.7722*0.514
}
if(user_input == "L"){
  user_input <- readline("> ")
  if(!is.na(as.numeric(user_input)))
    lines <- as.numeric(user_input)
  if(user_input == "D")
    lines <- 12
}
if(user_input == "I"){
  if(scroll>0)
    scroll <- scroll - 1
}
if(user_input == "O"){
  if(scroll<10)
    scroll <- scroll + 1
}
if(user_input == "Q")
  quit <- TRUE
}