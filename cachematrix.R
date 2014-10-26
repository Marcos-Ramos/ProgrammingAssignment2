## This function creates 4 functions (each of them is described below) 
## and puts these functions in a list, furthermore, it caches a 
## matrix in a variable to be read in a parent environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## function to set the matrix as an operator/variable that can be use in another
  ## environment and clean the cached inverse matrix
  set <- function(y) {                      
    x <<- y
    m <<- NULL
  }
  
  ## Function to get the matrix cached in the previous function 
  ## or entered in makeCacheMatrix function 
  get <- function() x                       
  
  ## Function to cache the caculated inverse matrix in a variable that can be 
  ## used in any parent environment
  setsolve <- function(solve) m <<- solve
  
  ## Function to retrieve the inverse matrix
  getsolve <- function() m                  
  
  ## Set the functions in a list object
  list(set = set, get = get,                
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function retrieves the matrix cached and computes the inverse matrix if possible
## if it is not posible to compute the inverse matrix, it shows a message clarifying
## the reason for.
## If you already have an inverse matrix cached, this function retrieves this matrix and
## show you the matrix cached.

cacheSolve <- function(x, ...) {
  
  ## Retrieve the cached inverse matrix, (if it is been already 
  ## computed, otherwise, retrieves NULL).  
  m <- x$getsolve()
  
  ## check if the inverse matrix is already cached!
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Retrieve the cached matrix to compute its inverse 
  data <- x$get()
  
  ## Verify if it is a matrix
  if(!is.matrix(data)) {
    stop("It is not a matrix.")
  }
  
  ## Verify if it is a square matrix
  if(!nrow(data)==ncol(data)) {
    stop("It is not a square matrix.")
  }
  
  ## Verify if the matrix is inversible, i.e., check if the determinant is zero.
  if (det(data)==0) {                
    stop("It is not an inversible matrix.")
  } 
  
  ## Calculate the inverse matrix.
  else {
    m <- solve(data, ...)
    
    ##Set the result to be printed.
    x$setsolve(m)
    m
  }
}