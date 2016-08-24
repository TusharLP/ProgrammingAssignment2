### Programming Assignment 2
## In this assignment I create two main functions:
## (1) makeCacheMatrix: creates a special matrix object
## that can cache its inverse
## (2) cacheSolve: computes the inverse of the matrix returned 
## by makeCacheMatrix. Retrieves the inverse from the cache 
## if it has been aleady calculated and the original matrix has not changed

## makeCacheMatrix defines a set of functions and returns them
## to the parent environment as a list:
## (1) set() assigns the input value to x
## (2) get() retrieves the value of x
## (3) setinverse() assigns the input value to the inverse object (inv)
## (4) getinverse() retrieves the value of the inverse object (inv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() gets the inverse from chache function or else finds the inverse 
## and stores it in chache function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- makeCacheMatrix$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- makeCacheMatrix$get()
  inv <- solve(data, ...)
  makeCacheMatrix$setmean(inv)
  inv
}
