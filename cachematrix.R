## Functions for operating on matrix objects
## with caching capability for inverse value
##
## made for programming assignment 2,
## R programming course, 20.12.2014

## Function to create our matrix object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Get the matrix
    get <- function() x
    # Set inverse matrix value
    setinv <- function(minv) inv <<- minv
    # Get inverse matrix value
    getinv <- function() inv
    # Return CacheMatrix object
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function to request inverse value. Retrieved value can be
## either precached or calculated on-the-fly

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    # Check if the value already calculated
    if(!is.null(inv)) {
        message("getting cached value")
        return(inv)
    }
    # Get the matrix
    data <- x$get()
    # Solve the matrix
    inv <- solve(data, ...)
    # Set the cache
    x$setinv(inv)
    # Return calculated value
    inv
}
