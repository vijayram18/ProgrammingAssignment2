## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "list" of functions & creates an empty matrix
## "set" functions takes data and creates the (non-empty) matrix
## "get" retrieves the matrix
## "setinv" saves the inverse to a value xinv
## "getinv" retreives the inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv = NULL
  set <- function(data) {
    x <<- data
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## the function retrieves the inverse and check if it is NULL
## if not NULL, it will return the inverse
## otherwise, it will get the data (assuming set outset of this function)
## the solve for the inverse, save it and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setinv(xinv)
  xinv
}
