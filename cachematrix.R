## Caching the inverse of a matrix
## Below are 2 functions that can be used to cache the inverse of a matrix

## The 1st function creates a special matrix object which is actually
## a list of 4 functions used to cache the matrix inverse:
## set function - sets the value of the matrix
## get function - gets the value of the matrix
## setsolve function - sets the result of matrix inversion
## getsolve function - gets the result of matrix inversion


makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) im <<- solve
  getsolve <- function() im
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## The 2nd function calculates the inverse of the special matrix created above
## If the inverse of matrix already calculated then it gets it from cache
## rather than calculating it again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getsolve()
  ## Does inverse of matrix already exist?
  ## if so then return it
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  ## if it doesn't exist then calculate it
  data <- x$get()
  im <- solve(data, ...)
  x$setsolve(im)
  im
}

# TESTING OF FUNCTIONS

# # Create test input matrix
# input_mat <- matrix(c(4, 2, 7, 6), nrow = 2)
# 
# # Feed test matrix to makeCacheMatrix
# test_mat <- makeCacheMatrix(input_mat)
# 
# # Feed output from above to cacheSolve
# cacheSolve(test_mat)
# 
# # Confirm this new matrix is actually the inverse of input matrix
# # Should return Identity matrix
# input_mat %*% cacheSolve(test_mat)

