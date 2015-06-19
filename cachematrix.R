## Put comments here that give an overall description of what your
## functions do

## These two functions work together to store a matrix and 
## cache its inverse.  When the inverse is required subsequently
## a check is made to see if a cached value is
## available and if so it is returned, otherwise teh 
## inverse is calculated and cached

## Write a short comment describing this function

##  This function stores a matrix x and caches its inverse.  
## It return a list of functions which are passed to the
## cacheSolve function to enable it to get and set the inverse 
## from or to the cache.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

## This function takes as input the list of functions from 
## makeCacheMatrix and uses tehm to fetch the cacjed inverse
## if it is available or else to fetch teh data using get and calculate
## its inversd and store in the cache using setsolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
