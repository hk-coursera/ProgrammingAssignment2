## A pair of functions that allow matrix to cache result of solve function.
## makeCacheMatrix: constructs matrix that can cache it's inverse,
## when given as argument to next function.
## cacheSolve: returns inverse of a matrix,
## retrieving it from cache of given object if it's there
## or calculating it and storing it in cache if it's not.

## Constructs cacheable matrix (de facto - an object with get/set, getInverse/setInverse methods)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(newVal) {
        x <<- newVal
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newVal) inverse <<- newVal
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## returns result of solve function (from cache if possible)

cacheSolve <- function(x) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
    } else {
        inverse <- solve(x$get())
        x$setInverse(inverse)
    }
    inverse
}
