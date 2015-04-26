## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(Mat = matrix()) {
  ##Creates special matrix that can cache its inverse
  #define inverse as null
  inv <- NULL
  #create function y
  set <- function(y)
  {
    #get the Matrix
    Mat <<- y
    #set inv to null
    inv <<- NULL
  }
  #get the matrix
  get <- function() Mat
  setinv <- function(y) inv <<- y
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(Mat, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- Mat$getinv()
  
  #validate if we have null
  if (is.null(inv)) {
    message('Computing inverse...')
    #get matrix from parameter
    data <- Mat$get()
    #calculate the inverse
    inv <- solve(data, ...)
    #set value
    Mat$setinv(inv)
  } else {
    message('Returning cached inverse...')
  }
  #return value
  return(inv)
}