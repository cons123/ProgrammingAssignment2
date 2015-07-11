## file: cachematrix.R
## Provides implementation to support improved performance for determining
## matrix inverses through implementation of a cache mechanism.

## function: makeCacheMatrix
## Create matrix object suitable for use with cacheSolve function
##
## Args:
##   m_dat: matrix data for initialization.  default is a new matrix.
##
## Returns:
##   special matrix object represent by list of functions to manache the cache

makeCacheMatrix <- function(m_dat = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    m_dat <<- y  
    m_inv <<- NULL
  }
  
  get <- function() { m_dat }
  setinverse <- function(i) { m_inv <<- i }
  getinverse <- function() { m_inv }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function: cacheSolve
## Calculates the matrix inverse utilizing the cache when possible
## 
## Args:
##   x: 'special' matrix returned by call to makeCacheMatrix
##
## Returns:
##   The inverse matrix for a matrix created by the makeCacheMatrix
##   function.  The cached value will be returned if it has been already
##   calculated, otherwise it will be calculated and cached.

cacheSolve <- function(x, ...) {
  ## get cached value via getinverse.  inv will be null if cache is not populated.
  inv <- x$getinverse()
  if(!is.null(inv)){
    ## cache hit.  return cached value
    message("cache not null, returning cache value")
    return(inv)
  }
  ## cache miss.
  message("cache is null, calculating and populating cache")

  ## get matrix data and calculate inverse with 'solve'
  data <- x$get()
  inv <- solve(data, ...)
  
  ## set cached inversie via 'setinverse'
  x$setinverse(inv)

    ## return inverse
  inv
}
