# editR
#
filename <- ""
quit <- FALSE
lines <- 16
plot(0, 0, type = "n", xlim = c(0, lines), ylim = c(0, lines), col="white", family = "monospace", main = "", xlab = "", ylab = "H for help", axes = FALSE, frame.plot = FALSE)
size <- dev.size()
justify <- 1/(size[1]-1)
displayLines <- rep("a", lines)
for(i in 1:lines)
  displayLines[i] <- paste(rep("xyzy", i), collapse="")
user_input <- ""

while(!quit){
  
dispFilename <- filename
if(dispFilename=="")
  dispFilename <- "editR"

plot(0, 0, type = "n", xlim = c(0, lines), ylim = c(0, lines), col="white", family = "monospace", main = dispFilename, xlab = "", ylab = "H for help", axes = FALSE, frame.plot = FALSE)
for(i in 1:lines)
  text(nchar(displayLines[i])*justify,(lines-i),displayLines[i], family = "monospace")
user_input <- readline("> ")

if(user_input == "H")
  displayLines <- c("D: Inserts line","A: Inserts a line At a position", "UIOP: Moves cursor Left/Down/Up/Right", "H: writes out this help text","J: Adjusts Left-justify strength (D sets to default)","L: Sets line count","S: Saves to file","8","99999999999999999999","10","11","12","13","14","15","Q: Quits")
if(user_input == "J"){
  user_input <- readline("> ")
  if(!is.na(as.numeric(user_input)))
     justify <- user_input
  if(user_input == "D")
    justify <- 1/(size[1]-1)
}
if(user_input == "L"){
  user_input <- readline("> ")
  if(!is.na(as.integer(user_input)))
    lines <- user_input
  if(user_input == "D")
    lines <- 16
}
if(user_input == "Q")
  quit <- TRUE
}