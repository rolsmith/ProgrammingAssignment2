## Put comments here that give an overall description of what your
## functions do

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


## Write a short comment describing this function

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

my_matrix <- matrix(1:9,3,3)
my_matrix
makeCacheMatrix(my_matrix)
cacheSolve(my_matrix)
