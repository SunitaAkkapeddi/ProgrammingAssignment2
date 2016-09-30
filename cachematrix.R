## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## mInv - variable for Calculated Inverse Matrix
  mInv <- NULL
  set <- function(b) {
    x <<- b
    mInv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mInv <<- solve
  getinverse <- function() mInv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv <- x$getinverse()
  ## Check for cached value
  if(!is.null(mInv)) {
    message("Inverse calculated reading from cached data")
    return(mInv)
  }
  ## calculate inverse
  data <- x$get()
  mInv <- solve(data,...)
  x$setinverse(mInv)
  return(mInv)
}


