## Caching the inverse of matrix

## makeCacheMatrix creates a special "vector", which is really a list containing a function to 1 set the value of the matrix 2 get the value of the matrix 3 set the value of the inverse 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y){
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- inverse
		getinverse <-function() m
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## the following function calculates the mean of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been caculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calcuates the inverse of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        		message("getting cached data")
        		return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
