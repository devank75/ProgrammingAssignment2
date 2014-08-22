## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL #initialise inverse matrix
  
  #set the value of the matrix
  setOriginal <- function(matrix) {
    x <<- matrix
    inverseMatrix <<- NULL
  }
  
  #get the value of the matrix
  getOriginal <- function() {
    x # Matrix returned
  }

  #set the value of the inverse
  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  #get the value of the inverse  
  getInverse <- function() {
    inverseMatrix
  }
  
  # Return
  list(set = setOriginal, get = getOriginal, setInverse = setInverse, getInverse = getInverse)
}


## Takes the matrix returned by makeCacheMatrix and inverses it
## If it is already done then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # get inverse of 'x'
  oldMatrix <- x$getInverse()
  
  # Use the inverse if it is already done
  if( !is.null(oldMatrix) ) {
    message("cached is being acceesed")
    return(oldMatrix)
  }
  
  # Assign matrix
  actualData <- x$get()
  
  # matrix multiplication to calculate the inverse
  oldMatrix <- solve(actualData) %*% actualData
  
  #Set the inverse
  x$setInverse(oldMatrix)
  
  #Return
  oldMatrix
}
