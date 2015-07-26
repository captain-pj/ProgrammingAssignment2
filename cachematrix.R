## These functions essentially share data outside of their scope 
## using the <<- operator

## This will create a list containing a function to set and get the value
## of a matrix and its inverse. 
## example: mat <- matrix(c(1,3,2,4), nrow= 2, ncol =2)
## cachedMat <- makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This will make use of the list of functions in the previous function
## to calculate the inverse of the matrix. If it is already calculated,
## it will grab the inverse from the cache. If not, it will calculate it
## and set it in cache. Using the above example:
## cacheSolve(cachedMat) will return either the calculated inverse, or a
## message that it is retrieving the value from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}