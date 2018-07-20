## makeCacheMatrix and cacheSolve work together to provide the inverse of a
## square matrix and cache the inverse when the underlying matrix data has not
## changed

## makeCacheMatrix takes an existing square matrix and provides methods to 
## get and set the matrix, as well as solve for the inverse of the matrix
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  
  set <- function(newMatrix) {
    m <<- newMatrix
    inv <<- NULL
    
  }
  
  get <- function() m
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Takes a matrix wrapped by the makeCacheMatrix and calculates the inverse
## matrix. Once the inverse is calculate the result is stored in memory and 
## returned the next time call is made for the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  
}
