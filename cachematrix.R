## Program to create a "matrix" object and retrieve the inverse of the matrix if it has been already 
## calculated

## This function caches the matrix inverse. It creates a list of functions 
## (setmat, getmat, setinv, getinv), which are then used by cachesolve(). 
## x is the matrix and x_inv is its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    x_inv <- NULL
    setmat <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    # Get matrix from the cache
    getmat <- function() x
    
    setinv <- function(inverse) x_inv <<- inverse
    
    # Retrieve inverse from the cache
    getinv <- function() x_inv
    
    # return the created functions as a list to the working environment
    list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## Calculates the matrix inverse if it has already been not calculated 
## and stored in cache before

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    x_inv <- x$getinv()
    
    if(!is.null(x_inv)) {
        message("getting cached inverse matrix...")
        return(x_inv)
    }
    
    data <- x$getmat()
    x_inv <- solve(data, ...)
    x$setinv(x_inv)
    return(x_inv)
}

## Testing above code
B <- matrix(c(1,3,5,7,1,3,5,7,1),3,3)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
## Second time call to cacheSolve() prints the message and retrieves inverse from cache
cacheSolve(B1)
getting cached inverse matrix...
            [,1]        [,2]        [,3]
[1,] -0.08928571  0.03571429  0.19642857
[2,]  0.14285714 -0.10714286  0.03571429
[3,]  0.01785714  0.14285714 -0.08928571
