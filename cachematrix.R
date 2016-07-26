## The first function defines objects for a matrix and its inverse.  These objects will be stored and/or retrieved by the second function which calculates the inverse if not done already.  

## makeCacheMatrix allows a user to store a square matrix of numbers.  It also defines functions set() and get() that allow user to store a new matrix or access a stored matrix while within a different function (e.g. cacheSolve).   NOTE:  In this assignment, cacheSolve never accesses set().  makeCacheMatrix also defines functions setinv() and getinv() to store or access the inverse of the stored matrix, calculated by the second function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve calculates and returns the inverse of the original matrix stored by makeCacheMatrix.  If the original matrix has not changed (thus it and its inverse is already stored), then cacheSolve simply returns the existing inverse matrix rather than re-calculating it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

# EXERCISE
# Must input "data" to fill n x n spaces defined by square matrix with nrow = ncol = n;   Example here is using 2x2 matrix with values 4,2,7,6.  Expected inverse is 0.6, -0.2, -0.7, 0.4.
mtrx <- matrix (data=c(4,2,7,6),nrow=2, ncol=2)
# Process mtrx using makeCacheMatrix
mkcache <- makeCacheMatrix(mtrx)
# Process cacheSolve with the vector returned by mkcache <- makeCacheMatrix
cacheSolve(mkcache)