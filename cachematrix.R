
## The first function create a special matrix that has capability of storing its 
## inverse along with matrix itself. The second function takes input special
## matrix and uses it to cache inverse of matrix.

## this function encapsulates a matrix and returns a list of functions. Functions and description are given below:
## 1. set - used to set a matrix
## 2. get - used to get the matrix saved using 'set' function
## 3. setInverse - used to set the inverse of matrix
## 4. getInverse - used to get the inverse of matrix saved using 'SetInverse' function.

makeCacheMatrix <- function(x = matrix()) {

  ## cached inverse
  inverse <- NULL
  
  ## setter
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  
  ## getter
  get <- function() x
  
  ## inverse setter
  setInverse <- function(i) inverse <<- i
  
  ## inverse getter
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function takes the matrix created with above function and 
## and returns its inverse. Internally, if inverse is not calculated
## it calculates the inverse, caches its value and returns inverse. If inverse
## is already calculated it simply returns the inverse from cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
