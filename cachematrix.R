## The following functions cache the inverse of a (invertible) matrix.
## To prevent multiple costly computations of inverse of same matrix,
## the inverse is cached and returned as and when required.


## This function creates a special object, which is a list containing
## functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix, by computing the
## inverse or accessing the cached inverse (if it was calculated in past)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}
