## This pair of functions try to achive the PA 2

## Caches the inverse of the matrix for future computations

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   } # Establece los valores de la matriz 
   get <- function() x
   setm <- function(solve) m <<- solve # Obtiene la inversa de la matriz
   getm <- function() m
   list(set = set, get = get,
        setm = setm,
        getm = getm)
}


# Computes the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getm()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setm(m)
   m
}


#  --------------------------------------------------------------
# Proof that the code works
A = matrix(c(1,1,0,1,0,1,0,1,0), nrow = 3, ncol = 3)
cacheSolve(makeCacheMatrix(x = A))
