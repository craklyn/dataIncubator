# M represents the number of stacks
M <- 3
# N represents the number of disks
N <- 3
# Total weight of all disks
totalWeight <- sum(1:N)

# disks is an N-long array of positions for disks 1,2,..,N
initialGameState <- matrix(data=0, nrow=N, ncol=M)
initialGameState[,1] <- rep(1,N)


findValidMoves <- function(state) {
  returnVal <- list()
  
  for(peg in 1:M) {
    if(!any(state[,peg] == 1)) {next;}
    diskToMove <- min(which(state[,peg] == 1))
    
    # Consider all new peg positions except the disk's current position
    validMoves <- rep(0, M)
    newReturnVals <- lapply((1:M)[-peg], function(newPeg) {
      heaviestDiskOnNewPeg <- gameState[,newPeg]==1
      if(!any(heaviestDiskOnNewPeg) || min(which(gameState[,newPeg]==1)) > newPeg) {
        validMoves[newPeg] <- 1
        newBoard <- matrix(data=state,nrow=N, ncol=M)
        newBoard[diskToMove,peg] <- 0
        newBoard[diskToMove,newPeg] <- 1
        newBoard
      }
      
    })
    returnVal <- c(newReturnVals, returnVal)
  }

  returnVal[sapply(returnVal, is.null)] <- NULL
  returnVal
}

gameState <- matrix(data=0, nrow=3, ncol=3)
gameState[1,] <- c(0,1,0)
gameState[2,1] <- 1
gameState[3,1] <- 1
gameState
temp <- findValidMoves(gameState)
temp

# For each peg 1:M
  # Find the lighest disk (aka the diskToMove)
  # Find the positions that that disk can be moved
    # For each of those positions, draw a new game state.
    # Append returnVal with new game state
# Return a list of all new game states
