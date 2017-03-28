## These functions will store a matrix inverse in the cache for quick retrival
## of the matrix inverse.

## Creates helper functions to set & get the matrix and to set and get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Checks if the matrix inverse is stored in the cache and returns
## Otherwise calculates and stores the matrix inverse

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
            message("getting cached matrix inverse")
            return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setInverse(i)
        i
}
