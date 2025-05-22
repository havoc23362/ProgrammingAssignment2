## These functions cache the inverse of a matrix to avoid R repeating commands when it is done multiple time.

## Creates  a matrix object that catch inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  
    set <- function(y) {
        x <<- y     
        inv <<- NULL 
    }
    get <- function() x  # Return the matrix
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv  

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function returns the cached inverse, or computes and caches it otherwise.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse() 
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()        
    inv <- solve(data, ...)  
    x$setinverse(inv)        
    inv
}
