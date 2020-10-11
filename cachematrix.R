## The following functions create a special object that stores a matrix and its inverse

## This first function, creates a list cointaining a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This second function calculates the inverse of the special "matrix" created with the
## first function. However, it first checkes to see if the inverse has already been 
## calculated. If so, it gets the inverse frome the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

