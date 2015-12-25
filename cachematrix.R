#This code mimics the code sample given, adapting relevant variables and functions
#for matrix and matrix inversion rather than vectors and means

makeCacheMatrix <- function(x = matrix()) {
  #for a square invertible matrix, set and create it, then set and create the inverse
  #this is called by cacheSolve() if/when it needs to pull an inversion from memory
  matrix_obj <- NULL
  set <- function(y) { #assign values to object in a separate environment 
    x <<- y
    matrix_obj <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) matrix_obj <<- inverse 
  getinv <- function() matrix_obj
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  #returns the inverse of the matrix input to makeCacheMatrix()
  
  matrix_obj <- x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(matrix_obj)){ 
    #if inverse has been calculated already get it from cache
    message("getting cached data")
    return(matrix_obj)
  }
  #otherwise calculate it
  data <- x$get()
  matrix_obj <- solve(data, ...)
  
  x$setinv(matrix_obj)
  
  return(matrix_obj)
}