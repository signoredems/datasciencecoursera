## makeCacheMatrix - Add inverse and caching functionality to an invertible matrix
## cacheSolve - Return the inverse of a cache matrix

## Takes an invertible matrix and returns the matrix with inverse and caching functionality

makeCacheMatrix <- function(x = matrix()) {
  ## Define empty inverse matrix property
  m <- NULL
  ## Set x equal to matrix y and inverse matrix property m to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Return original invertible matrix
  get <- function() x
  ## Set inverse matrix property m equal inverse value
  setInverse <- function(inverse) m <<- inverse
  ## Return inverse matrix property
  getInverse <- function() m
  ## Return list of functions added to invertible matrix x
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of a matrix from cache if available or by solving

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## If inverse has already been computed, return cached value...
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## ...otherwise, get invertible matrix x, solve for inverse and cache the value
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  ## Return inverse matrix
  m
}
