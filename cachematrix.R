## This first function creates a framework to cache a given matrix calculated inverse.
## The second function checks to see if the given matrix calculated inverse exists.  If 
## it does it usese it.  If it does not exist it calculates it and then caches it.

## This function creates a matrix object that can cache its inverse.  It also create a list four
## functions to get and set the matrix and to get and set the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
## Return a matrix that is the inverse of 'x'

