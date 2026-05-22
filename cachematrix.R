## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  getInverse <- function() {
    ## make sure the matrix is square
    if(length(x)%%sqrt(length(x)) == 0) {
      inv <<- solve(x)
    }
  }
  list(get = get, getInverse = getInverse)
}


## Write a short comment describing this function 
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the matrix has been calculated above, then cacheSolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!is.atomic(x)){
    inv <- x$getInverse()
    if(is.null(inv)){
      message("getting cached data")
      return(inv)
    }
  } else {
    message("getting the inverse -- no cached data found")
    return(makeCacheMatrix(x)$getInverse())
  }
}

## non-atomic example
f <- makeCacheMatrix(matrix(1:4,2,2))
print(cacheSolve(f))

## atomic example
print(cacheSolve(matrix(1:4,2,2)))
