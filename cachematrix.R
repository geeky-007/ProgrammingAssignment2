## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is an invertible square matrix).

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y){
                x <<- y
                invrs <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {invrs <<- inverse}
        getInverse <- function() {invrs}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
##cacheSolve is a function which computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInverse()
        if(!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        matrx <- x$get()
        invrs <- solve(matrx, ...)
        x$setInverse(invrs)
        invrs
}
