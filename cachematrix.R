## These two functions work in conjunction to find the inverse of a matrix and
# set the inverse in the cache.

## makeCacheMatrix creates a list that 1) sets the value of the matrix,
# 2) gets the value of the matrix, 3) sets the inverse of the matrix,
# and 4) gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix, after first making sure that
# the inverse has not already been found. If it has been found, it will display
# the "getting cached data" message and retrieve the inverse; if not, it will
# compute the inverse and set it in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
