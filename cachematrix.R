## These functions calculate the inverse of a matrix and cache the result for 
## future use.

## A function to invert a matrix and cache the result, then to retrieve the
## cached result. Cache is set to NULL if a new matrix is passed as the argument.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieves the cached result of makeCacheMatrix function, or retrieves a new
## result and caches it for future use.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}

