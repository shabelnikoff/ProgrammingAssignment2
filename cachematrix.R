## This is a pair of functions that cache and compute the 
## inverse of a matrix.

## The function creates a matrix object that can cache
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
                set <- function(y) {
                        x <<- y;
                        inv <<- NULL;
                }
                get <- function() x;
                setinv <- function(inv) inv <<- inv;
                getinv <- function() inv;
                list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix function. In case the inverse has
## already been calculated, then the function should take 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data...")
                inv
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
