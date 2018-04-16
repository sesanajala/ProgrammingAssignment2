## This file contains a pair of functions for computing the inverse of a matrix with caching functionality
## It caches the computed inverse and returns a cached computational value if the function has been called earlier
## This helps in optimizing use of computing resources by not recalculating all the time


## This is a funtion to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y){
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) mInverse <<- solveMatrix
  getInverse <- function() mInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This is a function to compute the inverse of the special "matrix" returned by the function makeCacheMatrix above
## If the matrix inverse has already been computed, it returns the result from the cache
cacheSolve <- function(x, ...) {
  
  mInverse <- x$getInverse()
  
  ## check if inverse matrix is not null and return the value from the cache
  if(!is.null(mInverse)){
    message("getting cached data")
    return(mInverse)
  }
  
  ## inverse matrix does not exist, therefore compute it
  data <- x$get()             #get the original Matrix Data
  mInverse <- solve(data)     #compute the inverse of the Matrix
  x$setInverse(mInverse)      #set the inverted matrix
  mInverse                    #return the inverse matrix
}
