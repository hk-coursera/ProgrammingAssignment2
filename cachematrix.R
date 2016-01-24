## A pair of functions that allow matrix to cache result of solve function
## makeCacheMatrix: constructs cacheable matrix,
## cacheSolve: returns result of solve function, retrieving it from cache of given object if it's there
## or calculating it and storing it in cache if it's not

## Constructs cacheable matrix (de facto - an object with get/set, getInverse/setInverse methods)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(new_val) inverse <<- new_val
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## returns result of solve function (from cache if possible)

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
