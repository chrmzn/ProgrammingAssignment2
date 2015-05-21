## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create a special matrix that allows us to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      # We should confirm whether the new matrix is new or not
      # as we dont need to change anything if it's the same
      if(all(x == y)){
        message("Ignoring set as new matrix is the same")
      } else {
        x <<- y
        m <<- NULL
      }
    }
    # Return matrix 'x'
    get <- function() x
    # Assign solved inverse to m
    setinv <- function(solve) m <<- solve
    # Return the inverse of matrix 'x'
    getinv <- function() m
    # Return named attribute list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function

# Using the cache matrix object check if the inverse has been
# calculated returing the cached value if it has or by performing
# solve and caching the result

cacheSolve <- function(x, ...) {
  # Get the current value of the inverse
  m <- x$getinv()
  # If it's not NULL we have a cached value so return that
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise call solve() with any additional arguments 
  # provided to cacheSolve
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
