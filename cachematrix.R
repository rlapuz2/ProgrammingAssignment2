## Caching the inverse of a mstrix
## this two function project creates a matrix 
## and finds the inverse, taking advantage of 
## caching and saving computed inverses in local environments. 
## This was a project from Coursera's R, by R.Peng
## -- Ray Lapuz

## makeCacheMatrix
##   -- copied from makeVector. 
##   I simply changed the functions "mean" to "solve"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve
## -- takes advantage of cached inverses/matrices
## from the previous function. 

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
