## Assignment: Caching the Inverse of a Matrix
##This function creates a “matrix” object that can cache its inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        c1 <- NULL
        set <- function(y){
                x <<- y
                c1 <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) c1 <<- inverse
        getInverse <- function() c1
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        c1 <- x$getInverse()
        if(!is.null(c1)){
                message("getting cached data")
                return(c1)
        }
        
        m1 <- x$get()
        c1 <- solve(m1,...)
        x$setInverse(c1)
        c1
}
