## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function applies the same principle as the makeVector function in the caching the mean of a vector sample

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
        get <- function(){
            x
        }
        setinv <- function(inverse){
            inv <<- inverse
        }
        getinv <- function(){
            inv
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function applies the same principle as the cachemean function in the caching the mean of a vector samle. It only 
## uses the solve function as recommended to create the inverse of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        return(inv)     
}
