## This file contains two functions to:
## (1) create a vector of functions to get/set the value of a matrix and to get/set the inverse of that matrix
## (2) calculate the inverse of a matrix or get the result from cache when already calculated before 

## This function creates a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This functions calculates the inverse of a matrix
## It first checks to see if the inverse for this matrix has already has been
## calculated and was kept in cache. If so, it gets the value from cache and skips
## the calculation. Otherwise, it calculates the inverse of the matrix and puts 
## the value in cache via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
