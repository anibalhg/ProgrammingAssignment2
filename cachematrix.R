# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
	# parameter x contains a square matrix
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(i) inverse <<- i
	getInverse <- function() inverse
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

# This function retrieve the inverse of a matrix
cacheSolve <- function(x, ...) {
	# parameter x contains the special matrix object returned from makeCacheMatrix
	i <- x$getInverse()
	# If the inverse has already been calculated then retrieve the inverse
	# from cache
	if(!is.null(i)) {
			message("getting cached data")
			return(i)
	}
	# otherwise calculate the inverse matrix
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}