## 1.	Write function: makeCacheMatrix
## 2.	Write function: cacheSolve: 


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
im <- NULL
  setMat <- function(y) {
    x <<- y
    im <<- NULL
  }
  getMat <- function() x
  setinverse <- function(inv) im <<- inv
  getinverse <- function() im
  list(setMat = setMat,
       getMat = getMat,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of makeCacheMatrix and returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
       im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$getMat()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
