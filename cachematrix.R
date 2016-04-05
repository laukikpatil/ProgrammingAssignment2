## Put comments here that give an overall description of what your
## functions do

## Put comments here that give an overall description of what your
## functions do

## This function returns a list of 4 functions
## set the data
## get the data
## get the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function works in conjunction with the makeCacheMatrix function
## assumes the input is always a square matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  n <- x$get()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## Sample output 
##> m<-makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> y
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    5
##> n<-makeCacheMatrix(y)
##> cacheSolve(n)
##     [,1] [,2]
##[1,]   -5    3
##[2,]    2   -1
##> cacheSolve(n)
##getting cached data
##     [,1] [,2]
##[1,]   -5    3
##[2,]    2   -1
##> cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(n)
##getting cached data
##     [,1] [,2]
##[1,]   -5    3
##[2,]    2   -1