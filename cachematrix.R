## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function will create a unique object of the matrix with access to getter and 
## setter functions for inverse and data

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Calculates the inverse of the matrix through the special object created in the makeCacheMatrix function
## If inverse has not been calculated before, this function calculates it and stores it in the environment of the special object
## If previously used data is entered, then precomputed inverse is shown as it has been previously calculated and cached. 

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}

## Sample execution
## >source("cacheMatrix.R")
## > mat <- matrix(1:4,2,2)
## > xx <- makeCacheMatrix(mat)
## > cacheSolve(xx)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(xx)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 