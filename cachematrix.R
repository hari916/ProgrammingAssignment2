## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invm <- x$getInverse()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setInverse(invm)
        invm
}
