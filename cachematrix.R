## Put comments here that give an overall description of what your
## functions do
## These functions are used to invert a matrix and cache the result, by binding
## the original and inverted matrix together in the makeCacheMatrix() function. 
## The cacheSolve() function can then look up the inverted matrix bound to a 
## matrix, and returns that inverted matrix if it exists. If it does not exist,
## it calculates the inverse, and binds that to the original matrix using 
## setinverse()

## Write a short comment describing this function
## This function returns a list containing four functions, to set
## and get the original matrix, and to set and get the inverted matrix
## note that the functions have access to both matrices by the scoping rules
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of the matrix used to call
## makeCacheMatrix() above. It first checks if the inverted matrix 
## already exists (i.e. setinverse() was called before), if not it performs 
## the inversion, stores the result for future calls, and returns it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
