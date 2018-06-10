
## Matrix inversion is usually a costly computation. This file contains
## two functions that are used to create a special object that stores 
## a Matrix and caches its inverse.

## The purpose of the first function is to create a special "matrix" 
## object which contains a matrix and an empty cache for the
## inverse of the matix. 

## The second function checks the cache of the inverse and, if it
## is empty, calculates the inverse of the matrix, and caches it. If the cache is
## not empty the the function returns the contents of the cached instead of 
## re-calculating the inverse to save time.

## This function creates a special "matrix" object that can cache its inverse
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


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache. Otherwise the function calculates the inverse,
## caches it and returns the calculated inverse

cacheSolve <- function(x, ...) {
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
