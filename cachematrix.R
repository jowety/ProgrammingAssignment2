## R Programming: Programming Assignment 2
## Author: Jon Tyree
## makeCacheMatrix
## cacheSolve

## Takes an optional matrix object and returns a wrapper list object with get/set 
## functions for the matrix data and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Takes a matrix wrapper list object created by makeCacheMatrix
## and returns the inverse matrix, using the cache if available 
## or generating the value and setting the cache if it isn't generated yet.
## In my opinion, A cleaner way of doing this would be to put this code 
## directly in the getInv method of the wrapper object above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
