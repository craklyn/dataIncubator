library(dplyr)


# M represents the number of stacks
M <- 3
# N represents the number of disks
N <- 3
# Total weight of all disks
totalWeight <- sum(1:N)
# Total number of game moves
Tmax <- 16

# game states are represented as an N-long vector.  The value of 
# each index represents the position (peg) of that disk.

# Make a list of all possible game states
listGameStates <- function(input) {
  if(nchar(input) == N) {
    input
  } else {
    c(sapply(1:M, function(x) listGameStates(paste0(input,x))))
  }
}

gameStates <- listGameStates('')
gameBoard <- rep(0, length(gameStates))
names(gameBoard) <- gameStates

# Initialize board for first turn
gameBoard[[paste0(rep(1, N), collapse='')]] <- 1

# Initialize our TakeTurn Matrix, AKA "A".
takeTurn <- matrix(data=0, ncol=N^M, nrow=N^M)


for(col in 1:nrow(takeTurn)) {
  stateString <- names(gameBoard)[row]
  state <- as.numeric(strsplit(names(gameBoard)[row], '')[[1]])
  
  takeTurnRow <- rep(0, N^M)
  names(takeTurnRow) <- gameStates
  
  newStates <- list()
  for(peg in 1:M) {
    disksOnPeg <- (state == peg)
    if(!any(disksOnPeg)) {next;}
    
    diskToMove <- first(which(disksOnPeg))
    
    newStates <- c(lapply((1:M)[-peg], function(newPeg) {
      heaviestDiskOnNewPeg <- (state==newPeg)
      if(!any(heaviestDiskOnNewPeg) || last(which(heaviestDiskOnNewPeg)) > diskToMove) {
        newState <- state
        newState[diskToMove] <- newPeg
        newState
      } else {NULL}
    }), newStates)
  }
  # First, Remove NULLs from newStates
  newStates[sapply(newStates, is.null)] <- NULL
  newStates <- unlist(lapply(newStates, function(x) paste(x, collapse='')))
  newStatesTable <- table(newStates)
  for(i in 1:length(newStatesTable)) {
    takeTurnRow[[(names(newStatesTable)[i])]] <- newStatesTable[i]
  }
  
  takeTurn[,col] <- takeTurnRow
}

A2 <- takeTurn %*% takeTurn
A4 <- A2 %*% A2
A8 <- A4 %*% A4
A16 <- A8 %*% A8
A32 <- A16 %*% A16
A64 <- A32 %*% A32
A128 <- A64 %*% A64
A256 <- A128 %*% A128

# This is how we take turns:
# takeTurn %*% gameBoard














