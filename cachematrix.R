## Put comments here that give an overall description of what your functions do

## This function creates a special matrix to cache its inverse instead of 
## computing it multiple times

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinv = setinverse,
       getinv = getinverse)
}


## This function returns a matrix that is the inverse of 'x'. 
## It first checks whether the matrix inverse has already been computed. 
## If it has, the function gets the results and returns it. 
## Otherwise it computes the inverse and stores it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
