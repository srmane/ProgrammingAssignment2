## Objective is to write a pair of functions 
## that cache the inverse of a matrix
  
## makeCacheMatrix - Creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve - This function computes the inverse of the matrix returned
## by makeCacheMatrix. If the inverse has already been computed 
## and the matrix has not changed, then this function will return the 
## inverse from the cache.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached inverse of x")
    return(m)
  }
  ## Cache does not exist, compute the inverse, store in cache and return.
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
