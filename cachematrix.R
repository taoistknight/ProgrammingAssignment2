## makeCacheMatrix - creates a function that takes a matrix and defines functions to 
## - get the value of the matrix
## - set/modify the value of the initial matrix
## - get a cached inverse of the stored matrix, and
## - solve and cache the inverse of stored matrix

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) s <<- solve
	getinverse <- function() s
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Checks if a solution to the input matrix exists.  If it does, it returns the cached values, and otherwise solves the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		s <- x$getinverse()
		if(!is.null(s)) {
			message("getting cached data")
			return(s)
		}
		data<- x$get()
		s <- solve(data, ...)
		x$setinverse(s)
		s
}
