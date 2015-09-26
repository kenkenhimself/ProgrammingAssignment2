## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  invrs <- NULL
  set <- function(b) {
    a <<- b
    invrs <<- NULL
  }
  
  get <- function() a
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## This function computes the inverse of the special "matrix" created above
  invrs <- a$getInverse()
  if (!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  
  mat <- a$get()
  invrs <- solve(mat, ...)
  a$setInverse(invrs)
  invrs
}
