## This function creates a special "matrix" object that can cache its inverse.
# Written 7/17/2015 by mapleset
# for the R Programming course

makeCacheMatrix <- function(x = matrix())
{
  invValue <- NULL
  
  set <- function(y){
    x <<- y           #set internal matrix var
    invValue <<- NULL #clear the cached inverse solution
  }
  
  # get original matrix 'x
  get <- function() x
  
  setInverse <- function(solve) invValue <<- solve
  
  # get the cached inverse matrix (or null if not solved yet)
  getInverse <- function() invValue
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve retrieves the inverse from the cache.
# Written 7/17/2015
# for the R Programming course
##
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invValue <- x$getInverse()
  
  if(!is.null(invValue)){
    message("getting cached data")
    return(invValue)
  }
  
  #not 'solved' yet, do that now
  
  #get the original matrix 'x'
  data <- x$get()
  
  #solve for the inverse matrix
  invValue <- solve(data, ...)
  
  # 'cache' it in x
  x$setInverse(invValue)
  
  # return the inverse
  invValue  
}
