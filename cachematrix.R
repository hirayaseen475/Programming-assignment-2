## This function creates a matrix that caches the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL #sets inverse to be null until calculated
  
  #to set the matrix
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  #to get the matrix inverse from this function
  get <- function() x
  setmatrix <- function(solve) matrix_inverse <<- solve
  getmatrix <- function() matrix_inverse
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## to solve for the inverse, and retrieve cached inverse from 'makeCacheMatrix()' when called if it has already been calculated

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getmatrix() #to get the matrix inverse from the first function
  if(!is.null(matrix_inverse)) {
    message("getting cached data") #to print this message when retrieving cached data if already calculated
    return(matrix_inverse) #retrieve cached data if already calculated
  }
  data <- x$get() #to get initialized matrix
  matrix_inverse <- solve(data, ...) #solve for inverse
  x$setmatrix(matrix_inverse) #set inverse as new output value
  matrix_inverse #return the inverse
}

my_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)) #to initialize matrix for testing
cacheSolve(my_matrix) #first function run to calculate matrix inverse
cacheSolve(my_matrix) #to see if inversion was gotten from cache this time and not recalculated
my_matrix$getmatrix() #to see if cache was stored in 'getmatrix()' 
