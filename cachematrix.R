## Function to create Cache entry for a given matrix.
## Takes square matrix as an input and caches it. Returns Cache entry for specified matrix.
## Gives error if Matrix is not Square.
##
## get - returns original matrix
## set - sets matrix to new one. Resets cache for matrix inverse.
## getinv - returns cached value for matrix inverse
## setinv - caches matrix inverse
##
makeCacheMatrix <- function(m = matrix()) {
  if (nrow(m) != ncol(m)) {
    stop("'m' ", nrow(m), " x ", ncol(m), " must be square.")
  }
  inv <- NULL
  set <- function(y) {
    if (nrow(y) != ncol(y)) {
      stop("'m' ", nrow(y), " x ", ncol(y), " must be square.")
    }
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Accessor/inializer function for CacheEntry of the matrix.
## Checks if specified matrix inverse has been inialized and if not 
## calculates and sets it on the matrix cache entry.
cacheSolve <- function(m, ...) {
  
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}