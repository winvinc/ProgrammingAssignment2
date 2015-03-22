## Compution Inverse of Metrix is a costly computation and Time Consumption 
## This Function will use benefic of cachce to reduce computation repeatly 
## by caching the inverse of a matrix rather than compute it repeatedly. 
## There are two functions that used to cache the inverse of a matrix.


## makeCacheMatrix (x) Function
## This function creates a special "matrix" object that can cache its inverse.
## There have 4 subfunctions in this Function
## 1. set(newMetrix) = set the Metrix value
## 2. get() = get The Matrix value
## 3. setinverse(newInverseMetrix) = set The Inverse Metrix Value
## 4. getinverse() = get The Inverse Metrix Value
makeCacheMatrix <- function(x = matrix()) {
  inverseMetrix <- NULL
  set <- function(newMetrix) {
    x <<- newMetrix
    inverseMetrix <<- NULL
  }
  get <- function() x
  setinverse <- function(newInverseMetrix) inverseMetrix <<- newInverseMetrix
  getinverse <- function() inverseMetrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve (X,...)
## This function computes the inverse of the special Metrix
## and cache the Inverse Metrix to X
## Program is Work as follow step
## 1. checks to see if the Inverse Metrix has already been calculated. 
## 2.If Yes, it gets the Inverse Metrix from the cache and skips the computation. 
## 3.Else , it calculates the Inverse Metrix of the Metrix
## and sets the value of the Inverse Metrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  inverseMetrix <- x$getinverse()
  if(!is.null(inverseMetrix)) {
    message("inverseMetrix is Calculated")
    message("getting cached data.")
    return(inverseMetrix)
  }
  ValueMetrix <- x$get()
  inverseMetrix <- solve(ValueMetrix) ##solve(A) = Inverse of A where A is a square matrix.
  x$setinverse(inverseMetrix)
  inverseMetrix
}
