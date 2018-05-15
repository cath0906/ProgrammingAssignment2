## Programming Assignment 2
## CAJasareno 05.15.2018


## The following functions cache the inverse of a matrix 
## to lessen the computational time of getting the matrix inverse


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }	
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## pass the values in a list
}


## Function that computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## use $ to access values set in the list from makeCacheMatrix function
  
  m <- x$getinverse()
  if (!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}