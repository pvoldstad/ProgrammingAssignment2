## The functions contained in this file define a new data structure that
##  allows for potentially accelerating matrix inversion computations by
##  cacheing prior calculations as well as a new function to compute matrix
##  inversions taking the cached results into account.

## This function creates a data structure (list) that provides the following:
##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse of the matrix
##  4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function calcaluates the inverse of new matrix data structure.
##   It will first check to see if the inverse is already cached and
##   return it to avoid recalculating it.  If not stored, the inverse
##   is calculated, stored, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    solve <-x$getsolve()
    if(!is.null(solve)) {
        message("getting cached data")
        return(solve)
    }
    data <- x$get()
    solve <- solve(data, ...)
    x$setsolve(solve)
    solve
}

