## This code establishes two functions - makeCacheMatrix; cacheSolve
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated it retrieves it from the cache

rm(list=ls())

## This section establishes the function 'makeCacheMatrix'
## The output is a list caching the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## The following function outputs the inverse of the matrix
## by first checking the cache to see if the inverse
## has already been calculated.
## Otherwise it calculates the inverse from scratch.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("checking the cache")
    return(inv)
  }
  data <- x$getmatrix
  inv <- solve(data)
  x$setinverse(inv)
  return(inv)
  }
