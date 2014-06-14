## Inverting a matrix is a costly computing operation, so these functions
## compute the inverse of a matrix and cache it.
## Repeated invocation of the inverse function on a given matrix just
## returns the cached inverse saving valuable computing resources and time.

## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the
## inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        mx <- x$get()
        i <- solve(mx, ...)
        x$setinverse(i)
        i
}
