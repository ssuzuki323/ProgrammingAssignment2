## makeCacheMatrix creates a special "matrix" that can cache its inverse. cacheSolve computes
## the inverse of the special "matrix" but first checks to see if the inverse has already been
## cached, in which case it would skip the computation and return the inverse matrix.

## Create a special "matrix" object that can cache its inverse
## The returned object is a list containing functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse matrix, and get the value
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of matrix x. The function first checks to see if inverse has
## already been cached, in which case the function skips the computation and returns the
## inverse matrix. Otherwise, the function calculates the inverse matrix and sets the value
## of the inverse in the cache using setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
