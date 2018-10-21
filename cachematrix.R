## Calculate matrix inverses by caching previously calculted results.
##
## makeCacheMatrix function returns an object cosisting of a list of functionalities
## to store and retrieve matrices and their cached calcultated inverses
##
## cacheSolve function calculates the inversion of a matrix stored in one instance of the above
## mentioned object taking care of checking that the result is cached or not and storing 
## uncached results

## Creates a "matrix" object for x that can cache the inverse calculated out of it.
makeCacheMatrix <- function(x = matrix()) {
        
        ## The cached inverted matrix	  
	cachedInv <- NULL
        
	## Stores the matrix y and reset the cached value
        set <- function(y = matrix()) {
                x <<- y
                cachedInv <<- NULL
        }
 
        ## Returns the stored matrix
	get <- function() x
	  
	## Sets the cached inverse matrix
        setInverse <- function(toCache) cachedInv <<- toCache
        
	## Gets the cached inverse
	getInverse <- function() cachedInv

        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Computes the inverse of the "matrix" created by makeCachedMatrix retrieving the data from the
## cache when possible.
## Precondition: the matrix x invertible
cacheSolve <- function(x, ...) {
        
        ## Checks if the result is cached and return the cached value.
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Inverse not cached
	message("calculating and caching data")
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
