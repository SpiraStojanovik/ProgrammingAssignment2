## Matrix inversion is usually a costly computation and 
## there is some benefit to caching the inverse of a matrix rather 
## than computing it repeatedly. The following pair of functions cache 
## the inverse of a matrix.

## The function, makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse. The special matrix is a list containing a 
## function to do the following:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix

## the makeCacheMatrix function is used in the second function "cacheSolve"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the special "matrix" 
## created with the above function. 
## The function first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverese in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
