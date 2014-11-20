## The makeCacheMatrix takes a matrix and saves the matrix as an object. 
## The first time cachesolve is called it , makeCachematrix modifies the object to
## save its inverse, the second time cachesolve is called on the same object,
## makrCacheMatric returns the cached result instead of computing the inverse again.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve returns the inverse of the matrix made in makeCachematrix

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
