## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## while cacheSolve computes the inverse of the "matrix" object or uses the cached inverse


## creates special "matrix" object including functions
## to set/get the matrix, and set/get the inverse

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


## gets cached inverse matrix from special "matrix" object,
## or calculates and caches the inverse matrix if it is not already cached

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #getInverse will return NULL if it is not cached
  if(!is.null(inv)) {
    message("getting cached data") # prints message if inverse matrix is cached
    return(inv)
  }
  matrix <- x$getMatrix() #uses internal getMatrix function of the special "matrix" object
  inv <- solve(matrix) #use solve() function to calculate inverse matrix
  x$setInverse(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
