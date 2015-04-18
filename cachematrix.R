## -----------------------------------------------------------------
## R-programming assignment 2.
## This file contains two functions: makeCacheMatrix and cacheSolve.
## author: Andy Shiue
## -----------------------------------------------------------------

## This function will create a special 'vector' object which contains
## function to:
## 1. set matrix
## 2. get matrix
## 3. set inverse matrix
## 4. get inverse matrix
## parameter: x, assume invertible matrix

makeCacheMatrix <- function(x = matrix()) {
	## upon function call we clear the previous inverse
	## and initializes all the functions and return them
	## in a list.
	inv <- NULL

	## this is the set matrix function.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	## this is the get matrix function.
        get <- function() x

	## this is the set inverse matrix function.
        setInverse <- function(inverse) inv <<- inverse

	## this is the get inverse matrix function.
        getInverse <- function() inv

	## returning the list of functions.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will calculate the inverse of the matrix. If the
## inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## parameter: x, list of functions from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## get the inverse matrix from cache
	inv <- x$getInverse()

	## check if the inverse from cache actually exists.
	## Return the inverse matrix if it exists.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

	## Code continues if the inverse matrix doesn't exist.
	## We get the matrix and compute the inverse with solve()
	## and then set it to the cache and returns the inverse
	## matrix.
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
