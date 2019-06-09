# #https-github.com-rdpeng-ProgrammingAssignment2
##CacheMatrix
## A set of 2 functions that will cache a matrix's inverse

##These functions creates a special object of class matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y){
      x <<- y
      inve <<- NULL
    }
    get <- function() x
    setInverse <- function(solveinverse) inve <<- solveinverse
    getInverse <- function() inve
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }

## This function will  calculate the inverse of the matrix returned by makeCacheMatrix function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inve <- x$getInverse()
    if(!is.null(inve)){
      message("we will get cached data")
      return(inve)
    }
    data <- x$get()
    inve <- solve(data)
    x$setInverse(inve)
    inve      
  }
  
