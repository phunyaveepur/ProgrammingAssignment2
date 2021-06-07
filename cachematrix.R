## Caching inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInvert <- function() i <<- solve(x)
        getInvert <- function() i
        list(set = set, 
             get = get,
             setInvert = setInvert,
             getInvert = getInvert)
}


## Compute the inverse of matrix from makeCacheMatrix
## either from cache if calculated or new calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvert()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInvert(i)
        i
}
