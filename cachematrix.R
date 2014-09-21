## Special matrix object with functionality to cache inverse
## calculations to avoid repeated work

## Special matrix object with a set of built in functions as follows
##   get() - return value of matrix
##   set() - set value of matrix
##   getinverse() - get cached inverse
##   setinverse() - solve inverse and store in internal variable
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # init matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # solve matrix and store
  setinverse <- function(solve) m <<- solve
  
  # get previously solved result
  getinverse <- function() m
  
  # return special object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cached version of solve (inverse) calculation for special matrix 'x'. 
## Computes the value the first time we see the data, but will use a cache
## on repreated calls
##
cacheSolve <- function(x, ...) {

  # Try to get cached value
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Cache fetch failed, compute the value and store in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

## Test Matrix and functions
cacheTest <- function()
{
  message("init special matrix with:")
  sourceMatrix = matrix(c(1,2,3,4), nrow=2, ncol=2)
  print(sourceMatrix)
  amatrix = makeCacheMatrix(sourceMatrix)
  
  message("Test get() function: ", appendLF = FALSE)  
  getResult = amatrix$get()
  message(ifelse(identical(getResult, sourceMatrix), "pass", "fail"))
  
  message("Test cacheSolve() function: ", appendLF = FALSE)  
  solveResult = cacheSolve(amatrix)
  message(ifelse(identical(solveResult, solve(sourceMatrix)), "pass", "fail"))
  
  message("Test getInverse() function: ", appendLF = FALSE)  
  getinnverseResult = amatrix$getinverse()
  message(ifelse(identical(getinnverseResult, solve(sourceMatrix)), "pass", "fail"))
  
  message("Test cacheSolve() function (with cache): ", appendLF = FALSE)  
  solveResult = cacheSolve(amatrix)
  message(ifelse(identical(solveResult, solve(sourceMatrix)), "pass", "fail"))

  message("update special matrix to:")
  sourceMatrix = matrix(c(0,5,99,66), nrow=2, ncol=2)
  print(sourceMatrix)
  amatrix$set(sourceMatrix)
  
  message("Test get() function: ", appendLF = FALSE)  
  getResult = amatrix$get()
  message(ifelse(identical(getResult, sourceMatrix), "pass", "fail"))

  message("Test getInverse() function before solve (expect NULL): ", appendLF = FALSE)  
  getinnverseResult = amatrix$getinverse()
  message(ifelse(identical(getinnverseResult, NULL), "pass", "fail"))

  message("Test cacheSolve() function (with cache): ", appendLF = FALSE)  
  solveResult = cacheSolve(amatrix)
  message(ifelse(identical(solveResult, solve(sourceMatrix)), "pass", "fail"))

  message("Test getInverse() function: ", appendLF = FALSE)  
  getinnverseResult = amatrix$getinverse()
  message(ifelse(identical(getinnverseResult, solve(sourceMatrix)), "pass", "fail"))
}



