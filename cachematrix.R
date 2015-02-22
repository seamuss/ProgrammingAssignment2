# cachematrix.R
# Tasks to complete the assignment
# makeCacheMatrix =>  function to extend a matrix by adding inverse 
#                     properties
# cacheSolve => function extending the core solve(x,...) function 
#               to make use of a cached inverse where available

#
# Create an extended matrix
# -get() gets the matrix
# -set(y) updates the matrix content
# -setinverse(inverse) set the matrix inverse
# -getinverse() get the inverse matrix
#
makeCacheMatrix <- function(x = matrix()) {
  # initialise the inverse to null
  i <- NULL
  # function to update the matrix
  set <- function(y) {
    x <<- y
    # ensure the inverse is returned to null
    # so cached inverse is not used
    i <<- NULL
  }
  # function to return the matrix
  get <- function() x
  # function to set the matrix inverse
  setinverse <- function(inverse) i <<- inverse
  # function to return the inverse
  getinverse <- function() i
  # Return the special extended matrix as a list 
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#
# Entended solve function to enable caching
# cacheSolve(x,...) where x,... are matrices
#
cacheSolve <- function(x, ...) {
  # return the cached matrix for x where possible
  i <- x$getinverse()
  # if a cached inverse exists, return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # set data to the matrix value in x
  data <- x$get()
  # use the core solve function as the inverse
  # is not cached yet
  i <- solve(data, ...)
  # update the cached matrix inverse in x
  x$setinverse(i)
  # return the inverse matrix
  i
}