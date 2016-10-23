## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). This pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initialise matrix inverse
    inv <- NULL
    
    # set and get methods for matrix and inverse stored
    # inside this special "matrix"
    set <- function(matrix) {
        x <<- matrix
        inv <<- NULL # initially inverse is not computed
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    # return method implementions of this Cache Matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    # try to get already cached matrix inverse
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if it is null, do the computation:
    # get the matrix stored in the special "matrix"
    # compute its inverse
    # and cache the result
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
