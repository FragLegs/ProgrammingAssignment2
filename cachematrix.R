## makeCacheMatrix() and cacheSolve() work together to cache the
## potentially expensive solve() function, which finds the
## inverse of an invertible matrix.

## makeCacheMatrix() turns an object (in this case, x, a matrix)
## into a list of 4 functions:
## x$get() retrieves the original object
## x$set(y) sets the value of the original object to y
## x$getinverse() retrieves a cached value associated with the original object -
##                in this case, the inverse of the matrix
## x$setinverse() sets a cached value associated with the original object -
##                in this case, the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        # create a list that holds 4 functions for caching a
        # value related to the matrix x
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

## cacheSolve(x) will return the inverse of the matrix x and cache the value
##               so that future calls to cacheSolve(x) will be greatly sped up
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
