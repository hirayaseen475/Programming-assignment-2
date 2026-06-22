makeCacheMatrix <- function(A = matrix ()) {
  inv <- NULL
  set <- function(B) {
    A <<- B
    inv <<- NULL
  }
  get <- function() A
  
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}

cacheSolve <- function(A, ...) {
  inv <- A$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- A$get()
  inv <- solve(data, ...)
  A$setinverse(inv)
  inv
  
}