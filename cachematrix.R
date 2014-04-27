## makeCacheMatrix creates a special matrix object
## cacheSolve calculates the inverse of the matrix
 
makeCacheMatrix <- function(x = matrix()) {
	## list containing a function to: set a matrix, get a matrix,
	## set the inverse of a matrix, get the inverse of a matrix
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 
cacheSolve <- function(x, ...) {
    ## The function cacheSolve returns the inverse of a matrix created with
	## the makeCacheMatrix function.
	## If the cached inverse is available, cacheSolve retrieves it.
	## If the cached inverse is not available, cacheSolve computes it then caches it and returns it.
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}
