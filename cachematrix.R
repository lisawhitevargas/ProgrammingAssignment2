## Two functions are created to cache the inverse of a matrix.  
## The makeCacheMatrix function creates a special matrix object
## that can cache its inverse.  The second function, cacheSolve, 
## will compute the inverse of the special matrix that is returned 
## by makeCacheMatrix.


## The function makeCacheMatrix creates a special matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(y) {
		x <<- y
		inv <<- NULL
	}
	get = function() x
	setinv = function(inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function cacheSolve computes the inverse of the special matrix
## returned by the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv = x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat.data = x$get()
	inv = solve(mat.data, ...)
	x$setinv(inv)
	return(inv)
}

## Code below is created for testing purposes

data = rnorm(1000000)
invdata = matrix(data, nrow=10, ncol=10)
head(data)
head(invdata)

