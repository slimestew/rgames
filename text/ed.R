#ed like in R

prompt <- FALSE
input <- ""
read <- 1
state <- 1
add <- 1
prevadd <- 1
end <- 0
sign <- 1
err <- 0
lasterr <- 0
autohelp <- FALSE
newAdd <- FALSE
bufferchanged <- FALSE
buffer <- list()
undo <- list()
filename <- ""

help <- c("Warning: buffer modified", "Invalid address", "Invalid command suffix", "No current filename", "Warning: buffer modified")
help[0] <- ""

x <- function(i){
  return(substr(input[1],i,i))
}

insert <- function(lst, pos, elem){
  if(pos < 1 || pos > length(lst) + 1)
    return(NA)
  if(pos == 1)
    return(c(elem, lst))
  if(pos == length(lst)+1)
    return(c(lst, elem))
  return(c(lst[1:(pos - 1)], elem, lst[pos:length(lst)]))
}

while(state==1){
  if(prompt){
    input <- readline("*")
  }else{
    input <- readline("")
  }
  
  newAdd <- TRUE
  suppressWarnings({
    
  for(read in 1:nchar(input)){
    if(state == 1){
      if(is.na(as.integer(x(read)))){
        if(x(read) == '-' || x(read) == '^') {
          sign <- -1
          if(nchar(input)==1)
            add = -1
        } else if(x(read) == '+' || x(read) == ' ') {
          sign <- 1
        } else if(x(read) == ',' || x(read) == '%') {
          if(newAdd)
            add <- 1
          newAdd <- TRUE
          state <- 2
          sign <- 1
        } else if(x(read) == ';') {
          state <- 2
          newAdd <- TRUE
          sign <- 1
        } else if(x(read) == '$') {
          add <- length(buffer)
          newAdd <- TRUE
          state <- 2
        } else {
          sign <- 1
          state <- 3
        }
      } else {
        if(newAdd){
          sign <- 1
          add <- 0
          newAdd <- FALSE
        }
        if(as.integer(x(read))>=0 && as.integer(x(read))<=9)
          add <- sign*add*10 + sign*as.integer(x(read))
      }
    }
    if(state == 2){
      if(is.na(as.integer(x(read)))){
        if(x(read) == '-') {
          sign <- -1
        } else if(x(read) == '+' || x(read) == ' ') {
          sign <- 1
        } else if(x(read) == ',' || x(read) == '%' || x(read) == ';') {
          #wait for the input to start
        } else if(x(read) == '$') {
          end <- length(buffer)
          newAdd <- FALSE
          state <- 3
        } else {
          state <- 3
        }
      } else {
        if(newAdd){
          sign <- 1
          end <- 0
          newAdd <- FALSE
        }
        if(as.integer(x(read))>=0 && as.integer(x(read))<=9)
          end <- sign*end*10 + sign*as.integer(x(read))
      }
    }
    if(state == 3){
      if(x(read) == 'Q')
        state <- 0
      if(x(read) == 'q'){
        if(bufferchanged && lasterr!=1){
          err <- 1
          lasterr <- err
          state <- 9
        } else {
          err <- 0
          lasterr <- 0
          state <- 0
        }
      }
      if(x(read) == 'P')
       prompt <- !prompt
      if(x(read) == 'h')
        cat(help[lasterr])
      if(x(read) == 'H')
        autohelp <- !autohelp
      if(x(read) == 'a'){
        while(input != '.'){
          input <- readline("")
          undo <- buffer
          if(input != '.'){
            buffer <- insert(buffer, add, input)
            bufferchanged <- TRUE
            add <- add + 1
          }
        }
        add <- add - 1
      }
      if(x(read) == 'd'){
        if(add<1 || end > length(buffer)){
          err <-2
          lasterr <- err
        } else {
          undo <- buffer
          bufferchanged <- TRUE
          buffer <- buffer[-c(add:max(add,end))]
        }
      }
      if(x(read) == '$'){
        state <- 2
      }
      if(x(read) == 'e'){
        if(add<1 || end > length(buffer)){
          err <- 3
          lasterr <- err
        } else {
          read <- read + 1
          sign <- 1
          state <- 4
        }
      }
      if(x(read) == 'E'){
        if(add<1 || end > length(buffer)){
          err <- 3
          lasterr <- err
        } else {
          read <- read + 1
          sign <- 2
          state <- 4
        }
      }
      if(x(read) == 'f'){
        if(add<1 || end > length(buffer)){
          err <- 3
          lasterr <- err
        } else {
          read <- read + 1
          sign <- 3
          state <- 4
        }
      }
      if(x(read) == 'r'){
        if(add<1 || end > length(buffer)){
          err <- 3
          lasterr <- err
        } else {
          read <- read + 1
          sign <- 4
          state <- 4
        }
      }
      if(x(read) == 'p'){
        if(newAdd) {
          cat(buffer[[prevadd]])
        } else {
          for(i in add:max(add,end))
            cat(paste(buffer[[i]],"\n", sep=""))
        }
      }
      if(x(read) == 'w'){
        if(read != nchar(input))
          read <- read + 1
        sign <- 5
        state <- 4
      }
      if(x(read) == ''){
        state <- 2
      }
      if(x(read) == '=')
        cat(max(add,end))
      if(state == 3)
        state <- 9
    }
    if(state == 4){
      if(x(read) == ' '){
        read <- read + 1
        state <- 5
      } else if(sign == 5) {
        state <- 5
      } else {
        state <- 9
        err <- 3
      }
    }
    if(state == 5){
      if(sign == 1){ #e
        if(bufferchanged && lasterr!=5){
          err <- 5
          lasterr <- err
          state <- 9
        }
        undo <- buffer
        buffer <- list()
        filename <- ""
        for(read in read:nchar(input)){
          filename <- paste(filename, x(read), sep="")
        }
        buffer <- readLines(filename)
        add <- length(buffer)
        cat(sum(nchar(buffer), length(buffer))) #counts newlines
        state <- 9
      }
      if(sign == 3){ #f
        filename <- ""
        for(read in read:nchar(input)){
          filename <- paste(filename, x(read), sep="")
        }
        cat(filename)
        state <- 9
      }
      if(sign == 4){ #r
        undo <- buffer
        buffer <- list()
        temp <- ""
        for(read in read:nchar(input)){
          temp <- paste(temp, x(read), sep="")
        }
        buffer <- insert(buffer, max(add,end), readLines(temp))
        add <- add + length(readLines(temp))
        cat(sum(nchar(readLines(temp)), length(readLines(temp)))) #counts newlines
        state <- 9
      }
      if(sign == 5){ #w
        if(filename == "" && read == nchar(input)){
          err <- 4
          lasterr <- err
        } else {
          if(read != nchar(input)){
            filename <- ""
            for(read in read:nchar(input))
              filename <- paste(filename, x(read), sep="")
          }
          bufferchanged <- TRUE
          writeLines(as.character(buffer), filename)
          cat(sum(nchar(buffer), length(buffer))) #counts newlines
        }
        state <- 9
      }
    }
  }
    
    if(state<9){
      if(add<0 || add>length(buffer)){
        err <- 2
        lasterr <- err
      } else {
        err <- 0
        if(state != 0)
          cat(buffer[[max(add,end)]])
      }
  }
    
  if(err>0 && x(read) != 'h' && x(read) != 'H' && state>0){
    cat("?")
    if(autohelp)
      cat(help[err])
  }
  
  if(state>1)
    state <- 1
  })
  
}