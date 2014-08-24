## The functions compute and cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse. 
## Input to this function is a square matrix.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL               ## set a null value for the inverse matrix
  
  ## set new matrix and reset inverse matrix to null
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() x                                  ## get matrix
  setinv <- function(inverse) xinv <<- inverse         ## set inverse matrix value into minv
  getinv <- function() xinv                            ## get inverse matrix value from minv
  
  ## list the functions so they can be accessed externally
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()           ## get inverse matrix
  
  ## if minv is not null, inverse matrix is already calculated, retrieve cached data
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  data <- x$get()               ## get matrix
  xinv <- solve(data, ...)      ## calculate inverse matrix
  x$setinv(xinv)                ## set inverse matrix into minv
  xinv                          ## print inverse matrix
}
