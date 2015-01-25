## Functions to create and compute the inverse of a square matrix
## It's assumed that the passed matrix can be inverted.

## Function to create a matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Function to calculate the inverse of a matrix created with makeCacheMatrix()

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
