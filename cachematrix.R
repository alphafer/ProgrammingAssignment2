## This R program provides a chache value of the matrix inversion. Matrix inversion are time-consuming computations. 
## While small matrix of 2*2 can be calculated manually, the computations get exponentially more complicated and almost impossible without
## a computer. If the inverse is part of a loop going through several matrix of several square dimensions, It will be a lot more
## efficient to cache the first time the inverse is calculated so the next time is not calculated again. 

## makeCacheMatrix returns a list of functions to set and get the matrix and its inverse.
## set: Set the value of the matrix
## get: Get the value of the matrix
## setInverse: Set the inverse of the matrix
## getinverse: Get the inverse of the matrix

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

## cacheSolve returns the inverse of the matrix. The function first check if the inverse value already exist and deliver the cache version.
## Otherwise the matrix is calculated using the R function solve() and stores the inverse value for future retrieval.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    print("getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}