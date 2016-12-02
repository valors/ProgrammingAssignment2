## Computing the inverse to a matrix is time consuming.
## The functions below allow to cache the value of a matrix inversion as well as retrieve it.
## If the matrix inversion has already been calculated, there is no need for calculating it again.  

## This function creates a matrix. This matrix can then be used as argument for the cacheSolve function
## in order to calculate its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversion) inverse <<- inversion
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse of a matrix is already cached (saved)
## if it is, it returns its value. If it is not, it calculates it and returns its value as well.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
