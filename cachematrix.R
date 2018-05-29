## Put comments here that give an overall description of what your
## functions do

## This function will create a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
}
  get <- function() x
  setinv <- function(inverse) z <<-inverse
  getinv <- function() z
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function will compute the inverse of the matrix created. It will retrieve the inverse from the cache if the inverse has already been calcualated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getinv()
  if(!is.null(z)){
    message("getting cached data")
    return (z)
  }
  dat <- x$get()
  z <- solve(dat,...)
  x$setinv(z)
  z
}
