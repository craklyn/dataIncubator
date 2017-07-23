library(dplyr)
library(Hmisc)
library(pryr)
library('Matrix')
options(digits=10)

# M represents the number of stacks
M <- 6
# N represents the number of disks
N <- 6
# Total weight of all disks
totalWeight <- sum(1:N)


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
gameBoard <- rep(0L, length(gameStates))
names(gameBoard) <- gameStates

# Initialize board for first turn
gameBoard[[paste0(rep(1L, N), collapse='')]] <- 1

# Initialize our TakeTurn Matrix, AKA "A".
#takeTurn <- matrix(data=0, ncol=N^M, nrow=N^M)
takeTurn <- Matrix(data=0L, ncol=N^M, nrow=N^M, sparse=TRUE)
object_size(takeTurn)

for(col in 1:ncol(takeTurn)) {
  print(paste0("Now computing column ", col, " of ", ncol(takeTurn)))
  stateString <- names(gameBoard)[col]
  state <- as.integer(strsplit(names(gameBoard)[col], '')[[1]])
  
  takeTurnCol <- rep(0L, N^M)
  names(takeTurnCol) <- gameStates
  
  newStates <- list()
  for(peg in 1:M) {
    disksOnPeg <- (state == peg)
    if(!any(disksOnPeg)) {next;}
    
    diskToMove <- first(which(disksOnPeg))

    newStates <- c(lapply(c(peg-1, peg+1), function(newPeg) {
      disksOnNewPeg <- (state==newPeg)
      
      if(newPeg < 1 || newPeg > M) {
        NULL
      } else if(!any(disksOnNewPeg) || last(which(disksOnNewPeg)) > diskToMove) {
        # The last disk on the newpeg is the heaviest disk on it.  
        # If the heaviest disk is heavier than the disk to move, then 
        # the disk to move cannot be moved to the new peg.
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
    takeTurnCol[[(names(newStatesTable)[i])]] <- newStatesTable[i]
  }
  
  takeTurn[,col] <- takeTurnCol
}

#A1 <- takeTurn
#A2 <- A1 %*% A1
#A2 <- A2 / sum(A2 %*% gameBoard)
#A4 <- A2 %*% A2
#A4 <- A4 / sum(A4 %*% gameBoard)
#A8 <- A4 %*% A4
#A8 <- A8 / sum(A8 %*% gameBoard)
#A16 <- A8 %*% A8
#A16 <- A16 / sum(A16 %*% gameBoard)
#A32 <- A16 %*% A16
#A32 <- A32 / sum(A32 %*% gameBoard)
#A64 <- A32 %*% A32
#A64 <- A64 / sum(A64 %*% gameBoard)
#A128 <- A64 %*% A64
#A128 <- A128 / sum(A128 %*% gameBoard)
#A256 <- A128 %*% A128
#A256 <- A256 / sum(A256 %*% gameBoard)

# Okay final state after T=16 turns is:
# A16 %*% gameBoard
# and final state after T=256 turns is:
# A256 %*% gameBoard
# Now we need to find the moment of each of those states.

getMoment <- function(stateString) {
  state <- as.integer(strsplit(stateString,'')[[1]])

  moment <- 0
  for(i in 1:length(state)) {
    # Moment from disk i is weight * position.  Position 1 has x=0, so subtract 1.
    moment <- moment + i*(state[i]-1)
  }
  moment
}

getCoM <- function(stateString) {
  getMoment(stateString) / totalWeight
}


# Tmax: Total number of game moves
Tmax <- 16
steps <- log2(Tmax)

resultingMove <- takeTurn
if(steps > 0) {
  for(i in 1:steps) {
    print(paste0("Now squaring the move for move #", i))
    resultingMove <- resultingMove %*% resultingMove
    resultingMove <- resultingMove / mean(resultingMove)
  }
}

initialGameState <- gameBoard
resultingBoard <- as.numeric(resultingMove %*% initialGameState)
names(resultingBoard) <- names(initialGameState)
#resultingBoard

CoMs <- sapply(names(gameBoard), getCoM)

xm <- wtd.mean(CoMs, resultingBoard)
var <- wtd.var(CoMs, resultingBoard)
sd <- sqrt(var)

xm
sd

# For M=3, N=3, T=16: 
# > xm
# [1] 0.4322597717
# > sd
# [1] 0.3654019534
 
# For M=6, N=6, T=256:

