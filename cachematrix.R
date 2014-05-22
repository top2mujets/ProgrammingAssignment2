## Functions to store a matrix and cache its inverse if required
##   Use makeCacheMatrix to create cacheable matrix object
##   Use cacheSolve to retrieve matrix inverse from cache, or calculate it
##       on-the-fly, cache it, and retrieve it if not already cached.

## Function to create the matrix along with getters and setters for matrix and inverse 
makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse with NULL
    inv <- NULL

    ## set matrix to input & initialize inverse with NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## getter for input matrix
    get <- function() x
    
    ## setter for caching inverse matrix
    setinv <- function(inverse) inv <<- solve
    
    ## getter for cached inverse matrix
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function that returns the inverse of the matrix
##   If the inverse is already cached, return the cached value
##   If the inverse in not yet cached, calculate it, cache it, and return value
cacheSolve <- function(x, ...) {
    ## Get the currently stored value, either NULL or cached inverse
    inv <- x$getinv()

    ## Check value is NOT null, return the value (which is the cached value)
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ## Otherwise, calculate inverse, fill cache, and return inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(inv)
    inv
}
