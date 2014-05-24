## Put comments here that give an overall description of what your
## functions do

# Use of functions in this unit:
# > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# > h8 <- hilbert(8)
# @result h8 = inversible square matrix 
# > x <- makeCacheMatrix(h8)
# > r <- cacheSolve(x)
# > r2 <- cacheSolve(x)
# getting cached data

## Write a short comment describing this function
# make matrix that can store the solve-result and return it if already set
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # memory variable to store previous result
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # store solve-result in memory
  setsolve <- function(solve) m <<- solve
  # get previous solve-result
  getsolve <- function() m
  # list of operations
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Write a short comment describing this function
# inverse square matrix (solve) or return cache-result for previous solve-action
# x = makeCacheMatrix-object !!
cacheSolve <- function(x, ...) {
  ## Return a matrix hat is the inverse of 'x'
  # get previous solve-result (null if not "solved" yet)
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # perform new solve-action on data
  data <- x$get()
  m <- solve(data, ...)
  # store solve-result into cache
  x$setsolve(m)
  m
}
