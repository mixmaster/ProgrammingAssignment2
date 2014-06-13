## Implement a set of R functions that are able to cache potentially
## time-consuming matrix inversion computations.


## Make a cache matrix for use with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return the inverse of a matrix. If there is a cached inverse available,
## directly return it. Otherwise, calculate the inverse, cache it, and then
## return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached inverse")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
