##This file contains an implementation of a matrix that caches its inverse

## This function produces a 'matrix' designed to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    #if the value of the matrix changes, clear the cached inverse value
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a cache-aware version of solve().  If a cached value is present, it will return it.
## otherwise it will calculate the inverse and cache the result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

