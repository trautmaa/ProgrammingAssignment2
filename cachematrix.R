## Implementation by Alex Trautman for JHU Course "R Programming"
## September 22, 2015
## Code is based on examples on course website written by
## 			Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
## These functions in concert may be used to more efficiently compute matrix inversions
## In a process involving many matrix inversions, information will be cached so that
## if any matrices are repeated we can access the cached inversion and save ourselves
## computation time.

## makeCacheMatrix
## Input: a matrix, x
## Output: a list of functions specific to that matrix (set(), get(), setinverse(), getinverse())
## This function returns a list of four functions:
## set() sets the value of the matrix
## get() returns the value of the matrix
## setinverse() sets the inverse of the matrix
## getinverse() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	## Define set() to set the value of the matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## Define get() to return the matrix
	get <- function() x

	## Define setinverse() to store - "cache" - the matrix's inverse
	setinverse <- function(inverse) i <<- inverse

	## Define getinverse() to return the inverse, i
	## i will be NULL if it hadn't been cached, otherwise i will be the inverse
getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve
## Input: list of functions from makeCacheMatrix
## Output: inverse of matrix stored in input
## This function returns an inverse of a matrix
## cacheSolve takes as input the list returned by makeCacheMatrix
## It uses those functions to return a value of the inverse of the matrix set by makeCacheMatrix
## This functions ensures that if the inverse has already been calculated,
## It will not repeat the calculation but instead return the previously
## cached inverse.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()

	## If inverse has been cached already, just look it up
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	## Otherwise, get matrix and compute inverse
	data <- x$get()
	i <- solve(data, ...)

	## Set cache the inverse
	x$setinverse(i)

	## Return the inverse
	i

}
