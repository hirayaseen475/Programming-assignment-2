## makeCacheMatrix is a function that can cashe the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function retrieves the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  getinv <- function() inv <<- solve(x)
  list(get = get,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  inv
}
