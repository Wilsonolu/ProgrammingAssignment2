 ## These functions are designed to cache the inverse of a matrix to avoid 
## the costly computation of repeatedly inverting the same matrix. 
## `makeCacheMatrix` creates a special matrix object that can cache its inverse,
## while `cacheSolve` computes the inverse and retrieves it from the cache if it 
## has already been calculated.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It returns a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate the cache
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  # Return a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Get the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}

