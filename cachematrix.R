## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL #inverse matrix
  
  setOriginal <- function(matrix) {
    x <<- matrix
    inverseMatrix <<- NULL
  }
  
  getOriginal <- function() {
    x # Matrix returned
  }

  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  getInverse <- function() {
    inverseMatrix
  }
  
  
  list(set = setOriginal, get = getOriginal, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Takes the matrix returned by makeCacheMatrix and inverse it
## I fit is already done then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  oldMatrix <- x$getInverse()
  
  if( !is.null(oldMatrix) ) {
    message("cached is being acceesed")
    return(oldMatrix)
  }
  
  actualData <- x$get()
  
  oldMatrix <- solve(actualData) %*% actualData
  
  x$setInverse(oldMatrix)
  
  oldMatrix
}
