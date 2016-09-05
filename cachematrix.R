## The first function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The second function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.


## The below function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## This function Set the value of the matrix, get the value of the matrix, set the value of inverse
## and get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
## Initialize
	invm <- NULL
	set <- function(y) {
                x <<- y
                invm <<- NULL
        }
	get <- function() x
        setInverse <- function(inverse) invm <<- inverse
        getInverse <- function() invm
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The below function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invm <- x$getInverse()
## Check if matrix inverse is available in cache
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
## Compute matrix inverse
        invm <- solve(data, ...)
        x$setInverse(invm)
        invm
}
