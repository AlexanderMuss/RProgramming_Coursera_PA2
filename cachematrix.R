## Funtions for the second programming assignment (just a number of example's modifications) 

##  This function caches matrix and it's inversion

makeCacheMatrix <- function(x = matrix()) {
     inversion <- NULL
     ## this are just useful functions from the example that allow 
     ## to cache matrix and it's inverse
     set <- function(y) {
          x <<- y
          inversion <<- NULL
     }
     get <- function() x
     setinv <- function(inv) inversion <<- inv
     getinv <- function() inversion
     output <- list(set = set, get = get, setinv = setinv, getinv = getinv)
     ## Final output of the first function
     output
}

## This function computes the inversion of the given 
## 'special" matrix or returned the result of previous 
## computation if the matrix isn't changed.

cacheSolve <- function(x, ...) {
     inversion <- x$getinv()
     ## if there is inverted matrix in cache program would show it
     if(!is.null(inversion)) {
          message("getting cached data")
          return(inversion)
     }
     ## In other case our function starts to compute inverse matrix
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(inversion)
     ## Final output
     inversion
}

