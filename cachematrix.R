
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initialize the variable
  
  set <- function(y) {
    x <<- y ## a new matrix is defined in cache
    inv <<- NULL   # clear the cache when a new matrix is setted
  }
  
  get <- function() x ##return the matrix that was saved.
  
  setinverse <- function(inverse) inv <<- inverse ##set the inverse on cache
  
  getinverse <- function() inv ##return the result when it called on cacheSolve function
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ##search if the inverse has already computed on cache
  
  ##if the matrix has not changed and the inverse is already computed.
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...) ##compute the inverse
  x$setinverse(inv) ##store the result in cache
  inv ##return the result
}

