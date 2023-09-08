## Put comments here that give an overall description of what your
## functions do

## This function makes a special matrix object that contains a list of functions:
##set(), get(), setinverse() and getinverse()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## This function computes the inverse function for the special matrix returned by
## the function makeCacheMatrix. If the inverse is already calculated and the matrix
## has not changed, the cachSolve function retrieve the solution from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        tmp <- x$get()
        inv <- solve(tmp, ...)
        x$setinverse(inv)
        inv
}


