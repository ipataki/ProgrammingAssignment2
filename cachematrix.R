## makeCacheMatrix function creates a special "matrix" object that can cache its inverse 
##and then cacheSolve calculates the inverse of the matrix.
## If the matrix inverse is already calculated, it will instead 
## find it in the cache and return it, skipping the calculation.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_matrix <<-inverse
  getinverse <- function() inv_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function.
## If the cached inverse is already available, cacheSolve retrieves it, if
## not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()
  if (!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  } else {
    inv_matrix <- solve(x$get())
    x$setinverse(inv_matrix)
    return(inv_matrix)
  }
}


