
## The pair of functions below were written to cache the inverse of
## a matrix so that this costly computation process does need to be
## repeated for the same inputs.

## The first function, makeCatcheMatrix, returns a list of four functions
## to first set and get a matrix, and then to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix()
    set <- function(y) {
        x <<- y
        i <<- matrix()
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, cacheSolve, first determines whether the inverse
## has been calculated or not. If it has, the function returns the
## already cached value, while if not, the function goes ahead to 
## solve the inverse and return the value.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.na(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
