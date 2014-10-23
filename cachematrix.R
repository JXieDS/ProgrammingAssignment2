## R Programming Assignment 2 (10-23-2014)
## Include two functions that cache the inverse of a matrix in order to reap the 
## benefit of caching the rather than compute it repeatedly 

## makeCacheMatrix(x)
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matX = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
              matX <<- y
              m <<- NULL
        }

        get <- function() matX
        setinv <- function(inv = matrix()) m <<- inv
        getinv <- function() m
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(x, ...)
## This function computes the inverse of the special "matrix" (from a list) returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x = list(), ...) {
        m <- x$getinv()

        ## If the inverse has been calculated, skip the computation
        if(!is.null(m)) {
              message("getting cached data")
              return(m)        
        }

        mdata <- x$get()
        m <- solve(mdata, ...)      ## Computing the inverse of a square matrix
        x$setinv(m)                 ## Important!
        m
}
