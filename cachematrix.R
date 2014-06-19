## Together, makeCacheMatrix() and cacheSolve() implement a wrapper to solve() that can cache its result.

## makeCacheMatrix() returns a list of functions that execute inside an environment where a matrix and its inverse inverse can be stored
## and retrieved.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() receives a list made by makeCacheMatrix() and attempts to return a cached matrix inverse.  If that
## fails, it calculates the inverse and sets the cached inverse value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
