## These pair of functions calculate inverse of a matrix,
## but first it checks wheather the previously calculated 
## and cached version of inverse is available or not.

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInv<- function(inv) xInv <<- inv
  getInv <- function() xInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



cacheSolve <- function(x, ...) {
  xInv <- x$getInv()
  if(!is.null(xInv)) {
    message("getting cached inverse matrix")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInv(xInv)
  xInv
}
