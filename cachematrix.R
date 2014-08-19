##
## This R script contains functions to cache a matrix inverse in an object for
## the purposes of saving the computation power (for example for the case of
## looping and repeated calculations). The first function is designed to create
## a list of "methods" (set and get the matrix, set and get the matrix inverse),
## the second function uses this "list" to save or get the cached matrix 
## inverse.

## Function 'makeCacheMatrix' takes a matrix as an argument (with an empty
## matrix by default) and creates a list of "methods":
##   1. "set" method to save an input matrix and reset any saved inverse
##   2. "get" method to load a matrix
##   3. "setinv" method to save an inverse matrix (passed as an argument, not 
##       calculated inside the function)
##   4. "getinv" method to load a saved matrix inverse if present
##
## Examples of usage:
##   myMatrix <- makeCacheMatrix(X) ... takes an existing square regular (thus
##        invertible) matrix X and creates the list of abovementioned methods
##   myMatrix$get() ... returns the matrix X
##   myMatrix$setinv(Xinv) ... sets the inverse of matrix X passed to the method
##        using a matrix called Xinv (calculated externally)
##   myMatrix$getinv() ... returns the matrix inverser Xinv or NULL (if no
##        inverse was saved to the object)

makeCacheMatrix <- function(x = matrix()) {
     xinv <- NULL
     set <- function(y) {
          x <<- y
          xinv <<- NULL
     }
     get <- function() x
     setinv <- function(inv) xinv <<- inv
     getinv <- function() xinv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv) 
}


## Function 'cacheSolve' takes the object created by 'makeCacheMatrix' and:
##   1) if the matrix inverse was not calculated, it calculate the inverse
##        and saves it to the object
##   2) if the matrix inverse was saved by this function in a previous
##        iteration, it returns the cached inverse

cacheSolve <- function(x, ...) {
     xinv <- x$getinv()
     if(!is.null(xinv)) {
          message("Getting cached data...")
          return(xinv)
     }
     data <- x$get()
     xinv <- solve(data, ...)
     x$setinv(xinv)
     xinv
}