## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## while cacheSolve computes the inverse of the "matrix" object or uses the cached inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
    
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$getMatrix()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
