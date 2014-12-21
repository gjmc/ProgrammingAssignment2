## The first block of functions handles caching a matrix x and its inverse invmatx
## The second block checks if the inverse of matrix alreay exists and returns it, otherwise
## it caclulates the inverse using solve() using th cached data

## This function creates a special "matrix" object x that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invmatx <- NULL
  setmatx <- function(y) {
    x <<- y
    invmatx <<- NULL
  }
  getmatx <- function() x
  setinvx <- function(inv) invmatx <<- inv
  getinvx <- function() invmatx
  list(setmatx = setmatx, getmatx = getmatx,
       setinvx = setinvx,
       getinvx = getinvx)
}


## This function computes the inverse invmatx of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatx <- x$getinvx()
  xcachd <- getmatx()
  if(!is.null(invmatx)) {
    message("getting cached data")
    return(invmatx)
  }
  data <- x$getmatx()
  invmatx <- solve(data)
  x$setinvx(invmatx)
  invmatx
}
