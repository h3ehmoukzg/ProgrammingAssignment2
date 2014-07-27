## thes two function uses a list in a special environment to cache 
## a matrix and the inversion of this matrix

## makeCacheMatrix creates the list and implements
## the functions to access the cached data
## the functions are:
## get, set, getinv, setinv
## the argument x is the matrix to be cached and inverted

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the inverse matrix
## it takes the values from the cached list if not empty
## otherwise it calculates the inverse matrix and stores
## the value into the cached list
## the argument x is the cached list created by makeCacheMatrix
## the return value is the the inverted matrix

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
