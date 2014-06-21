## This is Ying Li's submission for Assignment 2 for R Programming 
## The assignment is to write an R function that is able to cache potentially time-consuming computations
## The specific computation to be cached is the inverse of a matrix
## We assume for this assignment the matrix passed to the function is always invertible
## I followed the mean caching example given by instructor, substituted out the relevant pieces
## The R characteristics that was taken advantage of in this assignment is Lexical Scoping. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {	
                x <<- y
        	inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
