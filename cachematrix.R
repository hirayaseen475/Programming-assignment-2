## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y) {
        x <<- y 
        inv <<- NULL
    }
    
    get <- function() x # Función para obtener la matriz actual

    setinverse <- function(inverse) inv <<- inverse # Función para establecer (cachear) la inversa
    getinverse <- function() inv
    
    # Retorna una lista con todas las funciones
    list(set = set, 
         get = get,
         setinverse = setinverse, 
         getinverse = getinverse)


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()           # Obtiene la matriz
    inv <- solve(data, ...)   # Calcula la inversa con solve()
    x$setinverse(inv)         # Guarda en caché
    
    inv 
}
