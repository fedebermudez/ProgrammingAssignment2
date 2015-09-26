## This 2 functions calculate the inverse of a Matrix and store the result
## so you can fetch the result instead of repeating the calculation

## This is the function that creates the Matrix to be store
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) z <<- solve
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## This function inverts the Matrix or fetch it from the cache
cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
  }
