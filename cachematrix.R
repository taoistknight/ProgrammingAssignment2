## makeCacheMatrix - creates a function that takes a matrix and defines functions to 
## - get the value of the matrix
## - set/modify the value of the initial matrix
## - get a cached inverse of the stored matrix, and
## - solve and cache the inverse of stored matrix
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL									#creating variable to store cached matrix solution
	set <- function(y) {						# overwrites current stored matrix and clears cached solution
		x <<- y
		s <<- NULL
	}
	get <- function() x							#retrieves currently stored matrix
	setinverse <- function(solve) s <<- solve	#calls solve function to generate inverse of stored matrix
	getinverse <- function() s					#retrieves cached solution
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Checks if a solution to the input matrix exists.  If it does, it returns the cached values, and otherwise solves the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		s <- x$getinverse()
		if(!is.null(s)) {						#cached solution exists, print message and return cached value
			message("getting cached data")
			return(s)							#break out of function
		}
		data<- x$get()							#retrive stored matrix value, and...
		s <- solve(data, ...)					#plug into the solve() function to generate inverse matrix
		x$setinverse(s)							#cache the generated solution
		s
}
