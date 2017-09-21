## This script comprise of a pair of functions that cache the inverse of a matrix.

## The function makeCacheMatrix is used to create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve return the inverse of the matrix provided by the function makeCacheMatrix
## If the inverse has already calculated, then the function cacheSolve will be able to retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a  ## Return a matrix that is the inverse of 'x'
  
}
