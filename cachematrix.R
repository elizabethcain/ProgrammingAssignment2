## Overview:
##This programming assignment shows the advantages of using cached
## variables in the environment to prevent making the same calculations
## over and over again. In this example we are reading in a matrix
## and computing its inverse, unless that answer is already cached,
## in which case we display the cached answer.
##
## Example:
## a<-makeCacheMatrix(matrix(1:4, 2, 2))
## cacheSolve(a)
## cacheSolve(a)   ##Run cacheSolve(a) a second time.




## The makeCacheMatrix function reads in the matrix and does the caching of both 
## the matrix and its inverse. It does this through functions that
## it stores in a list of functions. This function assumes
## the matrix is a square matrix.

makeCacheMatrix <- function(x = matrix()) {

  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL  ## A new matrix has been entered, therefore empty the cache that held the inverse for the old matrix.
  }
  get <- function() x
  setinverse <- function(mat_inv) mi <<- mat_inv ## The inverse is only being set here, not being solved.
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The cacheSolve function reads in a matrix that was set by function makeCacheMatrix
## and checks to see if its inverse is already cached. If it is not, then 
## this function calls the solve() function available in R to compute the
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinverse(mi)
  mi
}
