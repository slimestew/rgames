par(bg = "white")
plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), col="white", xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE, asp=1)
symbols(0, 0, circles = 1, inches = 1.5, add = TRUE, xpd = TRUE)

time <- Sys.time()
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

angle <- function(hour, min, period=12){
  return(2 * pi * (hour / period) - min)
}

for (i in 1:12) #ticks
  lines(c(1.5 * cos(angle(i-1,0)), 1.4 * cos(angle(i-1,0))), c(1.5 * sin(angle(i-1,0)), 1.4 * sin(angle(i-1,0))), col = "black", lwd=1, xpd=1)

lines(c(0,0.35*sin(angle(hours+3+(minutes/60), pi/2))), c(0,0.35*cos(angle(hours+3+(minutes/60), pi/2))), col="black", lwd=3) #hour
lines(c(0,sin(angle(minutes+15+(seconds/60), pi/2, 60))), c(0,cos(angle(minutes+15+(seconds/60), pi/2, 60))), col="black") #min
lines(c(0,0.75*sin(angle(seconds+15, pi/2, 60))), c(0,0.75*cos(angle(seconds+15, pi/2, 60))), col="red") #sec