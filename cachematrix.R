## Functions defined below are aimed at caching the inverse
## of a matrix instead of computing it multiple times,
## hence making the computations quicker. 
## The matrix has to be square and invertible. 

##makeCacheMatrix creates a matrix object, which can be used
##for caching the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

  matrixInverse <- NULL   ##sets the value of the initial matrix object to null
  
  set <- function(y) {    ##sets the value of the matrix
    x <<- y
    matrixInverse <<- NULL
  }
  
  get <- function() x   ##gets the value of the matrix
  
  setsolve <- function(solve) matrixInverse <<- solve  ##sets the value of the inverse
  
  getsolve <- function() matrixInverse                 ##gets the value of the inverse
  
  list(set = set, get = get,       
       setsolve = setsolve, 
       getsolve= getsolve)                             ##a list of defined functions         
}
  
  



## cacheSolve function is used to compute the inverse of the matrix returned by makeCacheMatrix
## in case of the already computed inverse, the function returns inverse from the cache

cacheSolve <- function(x, ...) {
  matrixInverse <- x$getsolve()
  if(!is.null(matrixInverse)) { 
    message("retrieving inverse from cache")
    return(matrixInverse)         ##checks whether the inverse was previously calcuted
                                  ##returns it if it was calculated
  }
  data <- x$get()                 ##calculates the inverse of the matrix in case it was not cached
  matrixInverse <- solve(data, ...)
  x$setsolve(matrixInverse)
  matrixInverse                   ##returns a matrix that is the inverse of 'x'
  
        
}
