rm(a)
mm=matrix(c(2:5),2,2)
m=matrix(c(1:4),2,2)
## This function creates a special "myMatrix" object that can cache its inverse.

makeCacheMatrix <- function(myMatrix = matrix()) {
  ## The function creates a list of 4 functions,
  ## setMatrix -> will set the initial value of myMatrix and its inverse if myMatrix is new
  ## getMatrix -> will print myMatrix
  ## setInvMatrix -> will set invMatrix 
  ## getInvMatrix -> will print invMatrix  
  invMatrix <- NULL
  setMatrix <- function(y) {
    if(identical(y,myMatrix)){
      message("Matrices are the same,\n Use cacheSOlve() or getInvMatrix() to get its inverse:\n")
      
    }else{
      myMatrix <<- y
      invMatrix <<- NULL
    }
    
  }
  getMatrix <- function() myMatrix
  setInvMatrix <- function(solve) invMatrix <<- solve
  getInvMatrix <- function() invMatrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(myMatrix, ...) {
  ## In -> myMatrix
  ## Out -> invMatrix
  ## the function will calculate the inverse of myMatrix 
  ## if myMatrix is new and will cache it for later use if myMatrix does not change
  invMatrix <- myMatrix$getInvMatrix()
  if(!is.null(invMatrix)) {
    message("Inverse has been calculated -> getting cached data")
    return(invMatrix)
  }else {
    data <- myMatrix$getMatrix()
    invMatrix <- solve(data, ...)
    myMatrix$setInvMatrix(invMatrix)
    invMatrix
  }
    
  
}


