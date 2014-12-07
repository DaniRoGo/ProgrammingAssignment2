## The following functions allow to calculate the inverse of a matrix in
## an efficient way. The first time the inverse is calculated the result
## is cached, subsequent request to calculate the inverse will return the
## cached result.

## This function wraps the matrix to allow caching of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function calculates the inverse of a matrix that's stored
## in a list created using the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
