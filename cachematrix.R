## This function creates a matrix that caches the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## to solve for the inverse, and retrieve cached inverse when called if it has already been calculated

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

my_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)) #to initialize matrix for testing
cacheSolve(my_matrix) #first function run to calculate matrix inverse
cacheSolve(my_matrix) #to see if inversion was gotten from cache this time and not recalculated
my_matrix$getmatrix() #to see if cache was stored in 'getmatrix()' 
