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


for(stateString in names(gameBoard)) {
  state <- as.character(strsplit(temp, '')[[1]])
  
}
  
#takeTurn <- matrix(data=0, ncol=N^M, nrow=N^M)

# This is how we take turns:
# takeTurn %*% gameBoard














