# M represents the number of stacks
M <- 3
# N represents the number of disks
N <- 3

# disks is an N-long array of positions for disks 1,2,..,N
theDisks <- rep(1, N)
# Total weight of all disks
totalWeight <- sum(1:N)

calc_CoM <- function() {
  # We subtracts 1 from theDisks because the starting position of the problem is 0.
  sum(sapply(1:N, function(i) {i*(theDisks[i]-1)})) / totalWeight
}

doMove <- function() {
  #We want to make a list of all the valid moves, and then we'll choose one randomly
  
  # For each peg, find the lightest disk on it.  That's the disk that can be moved.
  for(peg in 1:M) {
    # Find all disks on this peg
    theDisksOnPeg <- theDisks == peg
    # If no disks are on this peg, move on to next peg
    if(!any(theDisksOnPeg)) {next} 
    # Find the lightest disk on this peg.  This is the top disk!
    topDisk <- min(which(theDisks==peg))

    
    
    # Find valid moves for the top disk.
  }

  
}

