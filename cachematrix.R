## 
## Caching the inverse of a matrix
##
## The file implements two functions:
##
##      The first  create a matrix object capable of internaly storing the value of
##         the matrix's inverse
##      The second that returns the inverse of a specified matrix held in a matrix,
##         using a cached value if possible or computing it otherwise.
##
## The matrix is assumed to be invertible.

##
## makeCacheMatrix create a matrix object, storing the specified R matrix and defining
## accessor functions.
##
makeCacheMatrix <- function(matrixData = matrix()) {

  cachedInverse <- NULL
  # method to stash the specified matrix and initialize the inverse to NULL to
  # indicate that the cache is empty
  set <- function(y) {
    matrixData <<- y
    cachedInverse <<- NULL
  }

  ## method to save the specified inverse value in the cache
  setinv <- function(inverse) cachedInverse <<- inverse

  ## methods to return the matrix and whatever is stored in the inverse cache
  get <- function() matrixData
  getinv <- function() cachedInverse

  ## the list of supported operations on the matrix pbject
  list( set=set, get=get,setinv=setinv,getinv=getinv)
}


##
## cacheSolve accepts a matrix object created with makeCacheMatrix and returns
## the inverse of that matrix
##

cacheSolve <- function(x, ...) {

  ## If inverse was previously cached, return it
  
  inverse <- x$getinv()
  if( !is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  ## Otherwise, compute, cache, and then return the inverse
  inverse <- solve(x$get())
  x$setinv(inverse)
  return(inverse)
}
