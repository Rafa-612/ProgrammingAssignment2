## These two functions work together to compute and cache the inverse of a matrix.
## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of that matrix, retrieving the cached
## result if it has already been computed.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set/get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Set a new matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL 
  }
  
  # Get the stored matrix
  get <- function() x
  
  # Set the cached inversed
  setinverse <- function(inversed_matrix) inverse <<- inversed_matrix
  
  # Get the cached inverse
  getinverse <- function() inverse
  
  # Return a list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then
## it retrieves the inverse from the cache instead of recomputing it.

cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inverse <- x$getinverse()
  
  # If inverse is already cached, return it with a message
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  # Otherwise, compute the inverse, cache it, and return it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
