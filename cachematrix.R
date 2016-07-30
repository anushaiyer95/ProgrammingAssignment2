## Both these functions have to do with caching the inverse
## of a matrix. Below are two functions that are used to produce
## a special object (matrix) that can cache its inverse.
## Caching the inverse of a matrix rather than computing it repeatedly
## may be more beneficial and less costly which is what I did below. 


## This function produces a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
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


## The function below generates the inverse of the above "matrix"
## created by function "makeCacheMatrix". If the inverse should have
## been calcuated already (matrix hasn't changed), then it should get
## the inverse from the cache 

cacheSolve <- function(x, ...) {
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

