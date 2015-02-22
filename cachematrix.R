## The functions in this file are used to create a cacheMatrix object
## that stores a matrix and caches its inverse matrix using the <<-
## operator.  The <<- operator can be used to assign a value to an
## object in an environment that is different from the current
## environment.
##
## These functions can be tested as follows:
##
## testMatrix <- matrix(c(4, 3, 3, 2), nrow = 2)
## cachedMatrix <- makeCacheMatrix(testMatrix)
## cacheSolve(cachedMatrix)
##
## Subsequent calls to the cacheSolve( ) function using the same cached
## matrix object result in the cached value being returned.



## makeCacheMatrix( ) creates a special "cacheMatrix", which is really
## a list containing functions to:
##
## 1.  Set the matrix
## 2.  Get the matrix
## 3.  Set the inverse of the matrix
## 4.  Get the inverse of the matrix
##
## Note:  The function to "set the matrix" is not needed to solve this
##        problem.  However, it is included here to ensure that
##        makeCacheMatrix( ) is more general purpose for future use.

makeCacheMatrix <- function(cachedMatrix = matrix( )) {

    # Initialize the cached inverse matrix so it has a NULL value the
    #    first time getInverse( ) is called.
    inverse <- NULL

    set <- function(x) {
        # Cache the matrix value x and set the cached inverse matrix
        #    to NULL
        cachedMatrix <<- x
        inverse <<- NULL
    }

    get <- function( ) {
        # Return the cached matrix value
        cachedMatrix
    }

    setInverse <- function(y) {
        # Cache the inverse value
        inverse <<- y
    }

    getInverse <- function( ) {
        # Return the cached inverse value
        inverse
    }

    # Create the list of functions to be returned
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve( ) returns the inverse of the special "cacheMatrix" input
## parameter that was created by makeCacheMatrix( ).
##
## It first checks to see if the inverse of the matrix has already been
## computed.  If so, it gets the inverse matrix from the cache and skips
## the computation.
##
## Otherwise, it computes the inverse of the matrix and sets the value
## of the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {

    # Get inverse matrix from cacheMatrix x
    inverse <- x$getInverse( )

    # If inverse is not null, return it
    if (!is.null(inverse)) {
        message("Getting cached data ...")
        return(inverse)
    }

    # Otherwise:

    # Get the matrix value from the cacheMatrix x
    matrix <- x$get( )

    # Calculate the inverse matrix
    inverse <- solve(matrix, ...)

    # Set the inverse matrix into the cacheMatrix x
    x$setInverse(inverse)

    # Return the inverse matrix
    inverse
}
