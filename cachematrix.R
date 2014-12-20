## The functions below allow you to potentially cut computation time on mean calculations if the data hasn't change.  
## It will use a cached mean instead of calculating, this is particularly useful in looping.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<-inverse
  getinverse <- function() m
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)}

##The following function calculates the mean of the special "vector" created with makeCacheMatrix. 
##It checks to see if the mean has already been calculated.
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
  message("cached inverse matrix")
  return(m)
  } 
  else {
  m <- solve(x$get())
  x$setinverse(m)
  m}}
