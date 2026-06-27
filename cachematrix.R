## These functions cache the inverse of a matrix so that the inverse
## is computed only once. If the inverse has already been calculated,
## the cached value is returned instead of recomputing it.

## Create a special matrix object that can store a matrix and cache
## its inverse.

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

## Return the cached inverse if available; otherwise compute the
## inverse, cache it, and return it.

cacheSolve <- function(x, ...) {
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
