## The first function makeCacheMatrix allows you to create a special Matrix 
## that can cache its inverse. The second function cacheSolve allows you to inverse a 
## special Matrix created with the makeCacheMatrix-Function. If there´s allready a cached
## solution (and no change to the matrix) cacheSolve() returns you the cached solution
## without recomputing the inversion.

## This function creates a special matrix based on the input x (x should be a matrix). 
## It allows you to re-set the matrix with the $set function and get the current matrix 
## with the $get function. Furthermore you can manually save a inverse-value with the 
## $setinverse function and get the currently saved inversve-value with the $getinverse 
## function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function inverses a special matrix created with the makeCacheMatrix-Function.
## If there is already a saved value for the inversion the function returns that saved
## value with the short message: "Getting cached data". However, if there isn´t a saved
## solution in the matrix, the function inverses the given matrix and caches the solution.
## You´ll notice that the function has inversed the matrix by the returned message: 
## "New solutions have been saved".

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)){
                message("Getting cached data")
                inv
        } else {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        message ("New solutions have been saved")
        inv
}}
