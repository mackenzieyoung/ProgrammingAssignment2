## Mackenzie Young

#this function takes in a matrix as input, and returns a list of four
#functions -- set, get, setinv, getinv, as well as the matrix x and its inverse inv
#makeCacheMatrix first sets the inverse to null,
#then the set function assigns y (its argument) to x, and resets inv to null
#the get function retrieves the data x
#the setinv function assigns solve (its argument) to inv, and getinv retrieves inv
#finally, the function creates a new list object containing these four functions
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#this function returns the matrix that is the inverse of x
#if inverse has already been comptued (i.e., it is non-null),
#cacheSolve retrieves the cached inverse, and does not recalculate it.
#if the inverse has not already been comptued, cacheSolve gets the data using get,
#calculates the inverse of the data using solve, and sets the inverse in the cache using setinv.
cacheSolve <- function(x, ...) {
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