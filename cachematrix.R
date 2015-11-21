
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a very heavy operation, thus it would make sense
## to caching the inverse of a matrix rather than compute it repeatedly.
## Doing so would save us a lot of time and make the process lean.
## Below are pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## The first function, makeVector creates a special "vector", which is really a
## list containing a function to 
##      - set the value of the vector
##      - get the value of the vector
##      - set the value of the mean
##      - get the value of the mean


## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}