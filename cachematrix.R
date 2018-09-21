## Calculating and caching the Inverse of a Matrix (assumtion: the matrix is always invertible)
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The function below creates special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    invMatrix <- NULL
    set <- function(y) {
          x <<- y
          invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) invMatrix <<- inv
    getInverse <- function() invMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The below function computes the inverse of the special "matrix" created by 
## makeCacheMatrix function above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
          message("getting cached matrix")
          return(invMatrix)
        }
        origMatrix <- x$get()
        invMatrix <- solve(origMatrix, ...)
        x$setInverse(invMatrix)
        invMatrix
}
