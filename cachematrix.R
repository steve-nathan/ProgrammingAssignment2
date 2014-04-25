## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## This pair of functions cache the inverse of a matrix.
## 
## Example begin
## > source("cachematrix.R")
## > m <- matrix(1:4,nrow = 2,ncol=2)
## > mcache <- makeCacheMatrix(m)
## > minverse <- cacheSolve(mcache)
## > minverse <- cacheSolve(mcache)
## getting cached data
## > minverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## The second invocation of cacheSolve(mcache) gets from cached data 
## 
## Example end

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

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


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

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
