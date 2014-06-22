# cacheMatrix.R
#
# Functions for caching the inverse of a matrix.
# Coursera "R Programming" course programming assigment 2.
#
# To test the code with a 2 x 2 test matrix:
#
# > source("cacheMatrix.R")
# > cachematrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
# > cachematrix$get()        # Shows the original matrix
# > cacheSolve(cachematrix)  # First run, computes inverse
# > cacheSolve(cachematrix)  # Second+ run, uses cached inverse
#

# Creates a special "matrix" object (a list really) that can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    
    # Functions to set and get the original matrix.
    set <- function(y) {
        x <<- y
        # New value for matrix -> reset inverse to NULL as it needs
        # to be recalculated.
        #
        invx <<- NULL
    }
    get <- function() x
    
    # Functions to set and get the inversed matrix.
    setinverse <- function(invmatrix) invx <<- invmatrix
    getinverse <- function() invx
    
    # Return a list with all our functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix().
# If the inverse has already been calculated earlier and the matrix has not
# changed, a cached inverse is used. 
#
cacheSolve <- function(x, ...) {
    invx <- x$getinverse()
    
    # The matrix has not changed and we have already calculated the inverse.
    if(!is.null(invx)) {
        message("Using cached inverse")
        return(invx)
    }
    
    # The matrix has changed, or this is the first time the inverse
    # is requested. Solve the matrix inverse and cache the result.
    #
    invx <- solve(x$get())
    x$setinverse(invx)
    invx
}
