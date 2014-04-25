## It takes a matrix as argument and returns list of functions

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
  
}


## This function uses the outcome from makeCacheMatrix to decide whether to run
# the computation or just take it from memory

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) { # Check if value is already in memory
    message("getting cached data")
    return(m)      # Print the value if in memory
  }
  data <- x$get()       # Get the input data
  m <- solve(data, ...) # Compute the inverse matrix
  x$setsolve(m)
  m                     # Print the outcome
}

# Usage ----
  mat <- matrix(rnorm(4),2,2)
  a <- makeCacheMatrix (mat)

  cacheSolve (a)
  cacheSolve (a)


