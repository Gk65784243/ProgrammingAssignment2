## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Fr improving of computing inverse of a matrix a cache
# mechanism is implemented.
# The first function "makeCacheMatrix" delivers a set of function 
# to handle save and get cached matrix.

# The second function "cacheSolve" handles computing a inverse of
# a matrix if the inversion of the matrix have not been computed 
# up to now. If the inverse of a matrix have already beend computed 
# before it reads the inverse via the list function provided by 
# function "makeCacheMatrix".

# Following list function are provided by "makeCacheMatrix":

#   - set: sets the value of the matrix
#   - get: gets the value of the matrix
#   - setinverse: inverse of the matrix is set
#   - getinverse: gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# Sample run:
 x = rbind(c(1, -1/8), c(-1/8, 1))
 print(x)
  
 print( class(x) ) 
 
 m = makeCacheMatrix(x)
 print(m)
 m$get()


# No cache in the first run
 cacheSolve(m)


# Retrieving from the cache in the second run
 cacheSolve(m)

