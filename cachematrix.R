## A module to invert square matricies while caching results.
## The cache, set up in makeCacheMatrix, avoids repeating 
## calculations by retrieving previously indexed results if available.

## makeCacheMatrix creates a list of functions that store the
## initial matrix (in $set), allow retrieval of the matrix (in $get),
## store the results of an inversion calculation (in $setInvert),
## and allow retrieval of the results (in $getInvert).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## store matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## retrieve matrix
    get <- function() x
    
    ## store result
    setInvert <- function(invert) m <<- invert
    
    ## retrieve result
    getInvert <- function() m
    
    ## create list
    list(set = set, get = get,
         setInvert = setInvert,
         getInvert = getInvert)
}


## cacheSolve first looks for a cached result using $getInvert()
## from makeCacheMatrix. If it is not already calculated and
## stored, it calculates the inversion using solve() and stores
## the result using $setInvert().

cacheSolve <- function(x, ...) {
    ## retrieve the result from x
    m <- x$getInvert()
    
    ## if the result exists, return the result and exit
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if the result does not exist, get the matrix from x
    data <- x$get()
    
    ## calculate the inversion
    m <- solve(data, ...)
    
    ## store the result
    x$setInvert(m)
    
    ## return the result
    m
}
