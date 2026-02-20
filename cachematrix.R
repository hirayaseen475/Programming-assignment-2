## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# It stores a matrix and its inverse in a private environment.
# The returned list contains functions to set and get the matrix,
# as well as to set and get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    m<<-inverse
  }
  
  getinverse <- function() {
    m
  }
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve computes the inverse of the matrix stored in the special object.
# If the inverse has already been computed, it retrieves the cached value.
# Otherwise, it calculates the inverse using solve(), caches it,
# and then returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
