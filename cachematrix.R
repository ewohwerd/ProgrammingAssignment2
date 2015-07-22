## Caches matrix inverse solutions
## Run makeCacheMatrix on matrix you wish to invert, then cacheSolve on the result of makeCacheMatrix
## cacheSolve will only run solve() if no cache exists for given matrix

## Creates a special object that can be used with cacheSolve to store and or retrieve solve() results for
## provided matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## when run on a makeCacheMatrix result, checks for a stored inverse, returns it if the matrix exists, or
## alternatively runs solve() and stores the result
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}