## The following functions can calculate and cache the 
## inverse of a given matrix

## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## this function stores a new matrix y into an 
    ## already existing special "matrix" object, setting
    ## inverse matrix m back to null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## this function returns the original matrix (x)
    get <- function() {
        x
    }
    ## this function stores the inverse matrix in m
    setsolve <- function(solve) {
        m <<- solve  
    } 
    ## this function returns the inverse matrix (m)
    getsolve <- function() {
        m
    }
    # Return the special "matrix" object
    list(set = set, get = get, 
         setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" object
## (returned by the above function) - or retrieves it from cache.

cacheSolve <- function(x, ...) {
    ## get inverse matrix
    m <- x$getsolve()
    
    ## If inverse matrix has already been calculated -
    ## retrieve it from cache:
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If inverse matrix has not been calculated yet -
    ## get the original matrix, calculate inverse, and
    ## set the result in the "matrix" object x
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)

    ## Return a matrix that is the inverse of 'x'
    m
}