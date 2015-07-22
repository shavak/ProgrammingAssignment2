## Shavak Sinanan
## R Programming Assignment 2
## Johns Hopkins University
## Data Science Specialization

## The following functions provide infrastructure to speed up the computation of the inverse of a matrix by caching already computed results.

## The following function creates a list of functions which allow the user to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # change the matrix
        x <<- y
        # the inverse needs to be reset
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    # reminds one of a Java object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function computes the inverse of the given matrix (in the form of a list as returned by makeCacheMatrix), accessing cached data if possible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        # inverse already computed
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    # save to cache
    x$setinverse(inv)
    inv
}
