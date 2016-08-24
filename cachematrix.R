## Creating a cache function for the inverse of a matrix taking advantage of the 
## scoping rules of R

## Cache function to store inverse matrix

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


## Function to get the inverse from chache function or else finding the inverse 
## and storing it in chache function

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
