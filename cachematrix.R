## The function 'makeCacheMatrix' creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solvedValue) m <<- solvedValue
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The function 'cacheSolve' computes the inverse 
## of the special "matrix" returned by the function 'makeCacheMatrix'
## If the inverse has already been calculated 
## (and the matrix has not changed), then the function 'cacheSolve' 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
