## makeCacheMatrix - Add inverse and caching functionality to an invertible matrix
## cacheSolve - Return the inverse of a cache matrix

## takes an invertible matrix and returns the matrix with inverse and caching functionality

makeCacheMatrix <- function(x = matrix()) {
  ## define empty inverse matrix property
  m <- NULL
  ## set x equal to matrix y and inverse matrix property m to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## return original invertible matrix
  get <- function() x
  ## set inverse matrix property m equal inverse value
  setInverse <- function(inverse) m <<- inverse
  ## return inverse matrix property
  getInverse <- function() m
  ## return list of functions added to invertible matrix x
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## if inverse has already been computed, return cached value...
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## ...otherwise, get invertible matrix x, solve for inverse and cache the value
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  ## return inverse matrix
  m
}
