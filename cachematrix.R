## These functions create a special matrix object that can cache its inverse
## and compute the inverse using caching for efficiency.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse as NULL
    inv <- NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse when matrix changes
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getinverse <- function() inv
    
    # Return a list of functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and matrix hasn't changed), 
## then retrieves inverse from cache
cacheSolve <- function(x, ...) {
    # Get cached inverse
    inv <- x$getinverse()
    
    # If inverse exists in cache, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setinverse(inv)
    
    # Return the inverse
    inv
}
