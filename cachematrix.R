## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) {
        inv <<- inverse
    }

    getinverse <- function() {
        inv
    }

    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function returns the cached inverse if available;
## otherwise it computes the inverse, stores it, and returns it.

cacheSolve <- function(x, ...) {

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
