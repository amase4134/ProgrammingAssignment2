## Put comments here that give an overall description of what your

## makeCacheMatrix creates a special matrix xa
## Then, sets the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

rm(list = ls())

makeCacheMatrix <- function(xa = matrix()) {
        ix <- NULL
        setMatrix <- function(y) {
                xa <<- y
                ix <<- NULL
        }
        getMatrix <- function() xa
        setinverse <- function(inv) ix <<- inv
        getinverse <- function() ix
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}
## Write a short comment describing this function

## cacheSolve function checks to see if the matrix inverse has been previously calculated
## if the matrix has been previously inversed, a message followed by the prior calculation will be returned
## otherwise the matrix will be inversed.

cacheSolve <- function(x, ...) {
        ix <- x$getinverse()
        if (!is.null(ix)) {
                message("patients, getting cached inverse matrix")
                return(ix)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
