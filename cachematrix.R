## the function creates a special object, 
## which seeks to project the inverse matrix.

## the idea is to set and get the matrix, then set and get the inverse matrix.

makeCacheMatrix  <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  } 
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}  

## The following function calculates the inverse matrix created with the 
## previous function and checks if the inverse matrix has been calculated. 
## if so, it gets the inverse matrix from the cache and skips the calculation.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

