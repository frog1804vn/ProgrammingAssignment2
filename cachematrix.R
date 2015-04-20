## Functions in this file aims at inversing a Matrix, also, they make the
## process less redundant by caching the insversion matrix in to a variable

## This function take a matrix as parameter, turning it into a matrix that
## can cached its inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(val) inv <<- val
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
