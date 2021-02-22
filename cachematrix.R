## These functions allow us to bypass calculations of inverse matrix if the calculation has been previously done and 
## has been stored in the cache

## creates an object to store matrix inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(a) inverse <<- a
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## check if the inverse has been calculated and stored in the cache. 
## If so, return that stored value. If not, computes the inverse and then return the calculated.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message('getting cached data')
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

