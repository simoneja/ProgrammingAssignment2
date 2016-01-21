## ******************************************************************
makeCacheMatrix <- function(mat = matrix()) {
 ## Cache a matrix and its inverse
  
  ## mat is a n x n matrix
  
  ## Return A list of functions as follows;
  ## setmat = stores the matrix in the cache 
  ## getmat = gets the matrix in the cache 
  ## setinv = stores the matrix inverse in the cache 
  ## getinv = gets the matrix inverse in the cache 
  ## ****************************************************************** 
  ## init varibles count as the number of legitimate rows and the sum
  inv <- NULL
  
  ## set matrix function
  setmat <- function(y) {
    ## set global variables
    mat <<- y
    inv <<- NULL
  }
  
  ## get matrix function
  getmat <- function() mat
  
  ## set inverse function as global
  setinv <- function(matInverse) inv <<- matInverse
  
  ## get inverse function
  getinv <- function() inv
  
  ## return function list
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}

## ******************************************************************
cacheSolve <- function(mat, ....) {
  ## Calculate a matrix inverse
  ## if cached then return result from the cache
  ## else calculate inverse and store item in the cache
  
  ## mat is a 'n x n' matrix
  
  ## Return matrix inverse
  ## ****************************************************************** 
  
  ## check cache 
  matInverse <- mat$getinv()
  
  if (!is.null(matInverse)) (
    message("getting cached data")
  )
  else
  {
    ## get matrix
    newmat <- mat$getmat()
    
    ## calculate inverse
    matInverse <- solve(newmat)
    
    ## update cache
    mat$setinv(matInverse)
  }
  
  return(matInverse)
}

