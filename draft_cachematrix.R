

my_matrix <- matrix(1:4,2,2)
my_matrix

solve(my_matrix)

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



cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  
  mat <- x$getmatrix()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  return(inv)

  }
my_matrix$getmatrix()
my_matrix$getinverse()
