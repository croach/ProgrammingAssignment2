## The makeCacheMatrix and cacheSolve functions work together to create a 
## special matrix wrapper object that can cache its own inverse value.

## The makeCacheMatrix function creates a special object that wraps a matrix
## object and provides accessor functions to the matrix object and the cached
## inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    # Clear the cached inverse whenever we update the matrix object to make 
    # sure we don't return an incorrect inverse.
    message("clearing the cached inverse")
    i <<- NULL
    x <<- y
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the given matrix and 
## caches it on that object. If the inverse of the give matrix is already 
## cached it returns instead of calculating it anew.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
