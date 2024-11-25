input <- readline(prompt = "Filename (no extension): ")
lines <- readLines(paste0(input, ".txt"))

joypos <- c(0,0)  #1 for up/right, -1 for down/left, assumes p1 side
lp <- FALSE
mp <- FALSE
hp <- FALSE
lk <- FALSE
mk <- FALSE
hk <- FALSE

keys <- strsplit(lines[1], NULL)[[1]]
# up, down, left, right, lp, mp, hp, lk, mk, hk
# todo: custom button definition
# todo: custom activation button
write("~Shift & Space::", file = (paste0(input, ".ahk")))
# 16.666 sleep