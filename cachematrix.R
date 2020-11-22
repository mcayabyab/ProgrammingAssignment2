##These functions create a special matrix, and also compute the inverse of this matrix

## The makeCacheMatrix function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) z <<- inverse
  getinv <- function() z
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special matrix returned by the makeCacheMatrix 
## Should return the inverse of the original matrix input
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getinv()
  ## check if inverse is NULL. Otherwise, return inverse value
  if(!is.null(z)){
    message("using cached data!")
    return(z)
  }
  DATA <- x$get()
  z <- solve(DATA, ...)
  x$setinv(z)
  return(z)
}
