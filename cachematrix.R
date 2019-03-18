## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that is invertible 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Checks if the inverse of the matrix has already been computed.  If so, it will forego computing it 
## again and just retrieves it.
## If not computed yet, then it computes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}