## This function creates and stores a special "matrix" object that can cache its inverse.
## the object is a list of four functions

makeCacheMatrix <- function(x = matrix()) {
    # store the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
    # retrieve the value of the matrix
  get <- function() x
  
    # store the value of the mean
  setInverse <- function(inverse) m <<- inverse
  
    # retrieve the value of the mean
  getInverse <- function() m
  
    # create and store the object: a list containing the 4 functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of a "matrix" 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    # first check to see if the inverse matrix has already been calculated and is cached
  inv <- x$getInverse()
  print(inv)
  if(!is.null(inv)) {
    message("Found in cache.")
    return(inv)   # retrieve and return immediately
  } else
  {
      # not yet cached: calculate the inverse of the input matrix 
    message("Not found in cache. Calculating it")
    data <- x$get()
    inv <- solve(data)
    # and set the value of the inverse matrix in the cache via the setInverse function.
    x$setInverse(inv)
    # return the calculated inverse matrix
    return(inv)
  }
}

## =========================================================================================
## This function is used only to test that the above code is working as supposed
##
## Test basic caching
##

unitTestCacheMatrix  <- function() {
  print("Start ... test matrix is: ")
  testMatrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
  print(testMatrix)
  
    # Testing set() and get(): create the special matrix object with caching functions
  matrixCached <- makeCacheMatrix(testMatrix)
  print("Get. This is what has been cached: ")
  print (matrixCached$get())
  
    # Test cacheSolve(); now invert the test matrix
  inverseMatrix <- cacheSolve(matrixCached)
  print("The matrix inversed is: ")
  print (inverseMatrix)
  print("multiplying the test matrix with its inverse should return the identity matrix: ")
  print(testMatrix %*% inverseMatrix)
  
    # Test caching mechanism. Now calculating again. It should found it in the cache
  print("Now calculating again. It should found it in the cache")
  againMatrix <- cacheSolve(matrixCached)
  
  if (!identical(inverseMatrix, againMatrix))
    message("Cached version does not match solved version")
}

