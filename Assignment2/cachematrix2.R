
## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}


##This is sample makeVector example given in the assignment to understand
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# This is sample cachemean() to understand the assignment
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

