## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# The makeCacheMatrix function is very similar to the vector example.
# It allows to set and get the matrix associated to the object, and to set and get its inverse.
# Note that when invoking the method set, it checks if the old matrix is identical to the new to avoid unnecessary computation.

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  
  set <- function(y) {
    if (!identical(x,y)) {
      x <<- y
      inverse <<- NULL
    }
  }
  
  get <- function() x
  
  set_inverse <- function(i) inverse <<- i
  
  get_inverse <- function() inverse
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
#
# The cacheSolve function is also as similar as possible to its vector counterpart.
# It first retrieves the inverse from x, checks if it is an actual matrix, and in case returns the cached value.
# Otherwise it computes the inverse with the method solve, and caches it on x for future retrievals.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  stored_inverse <- x$get_inverse()
  
  if (!is.null(stored_inverse)) {
    message("getting cached data")
    return(stored_inverse)
  }

  stored_matrix <- x$get()
  i <- solve(stored_matrix, ...)

  x$set_inverse(i)
  i
}

m <- matrix(c(1,2,3, 0,1,4, 5,6,0), nrow = 3, ncol = 3, byrow = TRUE)
m2 <- matrix(c(1,2,5, 0,1,4, 5,6,0), nrow = 3, ncol = 3, byrow = TRUE)
v <- makeCacheMatrix(m)
cacheSolve(v)
cacheSolve(v)