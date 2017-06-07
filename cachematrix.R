## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #set inverse matrix to Null
  inverse_matrix <- NULL
  #set function caches y matrix and sets inverse back to null
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_matrix <<- inverse
  getInverse <- function() inverse_matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve computes the inverse of the matrix.
## If the inverse has already been calculated then it displays the inverse stored in the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getInverse()
  if (!is.null(inverse_matrix)) {
    message("Output cached matrix")
    return(inverse_matrix)
  }
  mat <- x$get()
  inverse_matrix <- solve(mat, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
}

