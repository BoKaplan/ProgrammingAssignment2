## This is a pair of functions that will be used to create a special object that
## stores a matrix and caches its inverse.

## makeCacheMatrix is a function that creates a special "matrix" object which can
## cache its inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This second function computes the inverse of the special "matrix" created by
## makeCacheMatrix as shown above. If the inverse has already been calculated
## and the matrix has not changed, then the cacheSolve should retrieve from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
