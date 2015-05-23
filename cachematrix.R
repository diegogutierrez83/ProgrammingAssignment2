## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##  introduce the matrix x. 
  ##  example: a <-  makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
  
  m <- NULL
  set <- function(y) {
    ## use `<<-` to assign a value to an object in an environment 
    ## different from the current environment. 
    x <<- y    
    m <<- NULL
  }
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  ##if the inverse matrix has not been calculated then a$getsolve() given NULL.
}


#cacheSolve give the inverse of a matrix. The result is given to makeCacheMatrix. 
cacheSolve <- function(x, ...) {
## x is the output of makeCacheMatrix  
  
  if(det(x$get()) != 0){ ## Determinant of the matrix must be different than 0. Condition for Inverse.
  ## if the condition is satisfy then the program proceed to calculate the matrix inverse.
    m <- x$getsolve()
  if(!is.null(m)) {                 ## if the inverse has already been calculated
    message("getting cached data")  ## get it from the cache and skips the computation. 
    return(m)
  }
  data <- x$get()                ## otherwise, calculates the inverse.
  m <- solve(data, ...)
  x$setsolve(m)                  ## sets the value of the inverse in the cache via the setsolve function.
  m
  }
  else{print("your matrix is not invertible because determinant its equal to zero")}## if condition no satisfy.
  
  ## Return a matrix that is the inverse of 'x'
}
