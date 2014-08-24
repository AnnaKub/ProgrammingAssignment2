## Functions' purpose is to take a matrix and calculate its inverse. 
## If there was already calculation for specific matrix, function cacheSolve will not calculate 
## the inverse again, but will take the results from the cache(when calling again cacheSolve for previously calculated inverse).

## makeCacheMatrix is a function that takes a matrix as an input and stores it. Also after calling cacheSolve is stores inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve function calculates an inverse to matrix which was given in makeCacheMatrix function. 
## Firstly it checks if inverse hasn't been already calculated and is stored within makeCacheMatrix - if not it calculates inverse and returns it.
## If inverse to the matrix has been calculated and is stored than cacheSolve function inform user that it takes cached data and return the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
