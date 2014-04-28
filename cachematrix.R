makeCacheMatrix <- function(x = matrix()) {
  func <- function() x
  invert <- NULL
  setinv <- function(inverse) invert <<- inverse
  getinv <- function() invert
  return(list(func = func, 
              setinv = setinv,
              getinv = getinv))
}

 
cacheSolve <- function(x, ...) {
  invert <- x$getinv()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinv(invert)
  return(invert)
}