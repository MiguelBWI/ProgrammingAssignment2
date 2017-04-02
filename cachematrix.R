## 45678911234567892123456789312345678941234567895123456789612345678971234567898
## This function creates a special "matrix" object that is actually a  
## list of functions for saving and retreiving the inverse of a numeric 
## matrix passed into the function.  It uses the lexical scoping rules 
## to save its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    ## x is a square, numeric matrix passed into the function
    ##
    # internal variable pvar used to indicate change or pass data
    pvar <- NULL
    # define the set function
    set <- function(y) {
        # save (cache) the new value in the parent frame
        x <<- y
        pvar <<- NULL  #indicate change
    }
    # define the pass back function
    get <- function() x
    # define the 
    setinvx <- function(invX) pvar <<- invX
    getinvx <- function() pvar
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## the function retrieves the inverse from the cache.
## --> assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
    m <- x$setinvx()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- sort(data, ...)
    x$setinvx(m)
    m  ## Return a matrix that is the inverse of 'x'
}
