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
# At game start, all disks are on the first pet.
initialGameState <- paste(rep(1, N), collapse="")


findValidMoves <- function(state) {
  returnVal <- list()
  
  for(peg in 1:M) {
    disksOnPeg <- (state == peg)
    if(!any(disksOnPeg)) {next;}
    
    diskToMove <- first(which(disksOnPeg))
    
    returnVal <- c(lapply((1:M)[-peg], function(newPeg) {
      heaviestDiskOnNewPeg <- (state==newPeg)
      if(!any(heaviestDiskOnNewPeg) || last(which(heaviestDiskOnNewPeg)) > diskToMove) {
        newState <- state
        newState[diskToMove] <- newPeg
        newState
      } else {NULL}
    }), returnVal)
  }
  
  returnVal[sapply(returnVal, is.null)] <- NULL
  sapply(returnVal, function(x) paste(x, collapse=""))
}

temp <- findValidMoves(initialGameState)

theState <- c(1,2,3)
temp <- findValidMoves(theState)

# For each peg 1:M
  # Find the lighest disk (aka the diskToMove)
  # Find the positions that that disk can be moved
    # For each of those positions, draw a new game state.
    # Append returnVal with new game state
# Return a list of all new game states


gameState <- initialGameState
for(T in 1:Tmax) {
  print(paste0("Now handling T=",T))
  print(paste0("Size of gameState: "), length(gameState))
  gameState <- sapply(gameState, function(x) findValidMoves(as.numeric(strsplit(x, '')[[1]])))
  gameState <- unlist(gameState)
}
finalGameState <- gameState



mergeFrequencyTables <- function(x, ...){
  z <- Reduce(paste, list(x, ...))
  tsum <- table(strsplit(tolower(z), "\\W"))
  tsum
}

