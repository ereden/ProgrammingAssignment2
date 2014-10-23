## This function will create allow you to calculate the inverse of the matrix.
## If the matrix is unchanged, it will access the inverse of the matrix from 
## a cached value in order to save processing time. Note: this function only
## works on square invertible matrices.

## The makeCacheMatrix function generates a list that defines the matrix and
## its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function returns the inverse of the matrix x. If it has
## already been solved, the function returns the cached value of the inverse
## rather than re-solving the inverse.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached inverse")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}