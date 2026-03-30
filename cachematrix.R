## The following two functions find the inverse of a matrix.  
## If the matrix has already been inverted by cacheSolve, then 
## the solution is retrieved from cached values. 

## makeCacheMatrix sets the value of the matrix, gets the value of the matrix, 
## sets the inverse of the matrix and gets the inverse of the matrix. The 
## function then makes a list of 4 functions: set, get, getinverse, and setinverse.

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invert <<- inverse
        getinverse <- function() invert
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix.  If the matrix has already 
## been inverted by these functions, it returns the cached solution.  If 
## the input is a unique matrix cacheSolve solves for the inverse of the 
## matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getinverse()
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinverse(invert)
        invert
}