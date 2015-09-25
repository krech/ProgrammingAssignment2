## Functions for caching inverse matrix

## Creates matrix which supports caching inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

        get <- function() x

        set <- function(newMatrix) {
            x <<- newMatrix
            inverse <<- NULL
        }

        getInverse <- function() inverse

        setInverse <- function(newInverse) inverse <<- newInverse

        list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Stores inverse matrix in the object created by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
            return(inverse)
        }
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
        inverse
}
