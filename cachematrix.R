# makeCacheMatrix creates a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    #Create an empty vector
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    set.inv <- function(inverse)
        inv <<- inverse
    get.inv <- function()
        inv
    list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}
# This function computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$get.inv()
    # If the inverse matrix has already been calculated, get it from the cache and skip the computation
    if (!is.null(inv)) {
        message("getting cached data")
        inv
    }
    # Otherwise, compute the inverse matrix
    matr <- x$get()
    inv <- solve(matr, ...)
    x$set.inv(inv)
    inv
}