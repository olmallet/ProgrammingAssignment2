
## The makeCacheMatrix function takes a square matrix as input and returns an "enriched" matrix object
## that has the capacity to store its own inverse, in association with the companion function cacheSolve.
## The enriched object actually is a list of 4 functions  :
## - set : sets the matrix 
## - get : gets the matrix
## - setinverse : sets the inverse
## - getinverse : gets the inverse
## associated to a same R environment, which stores the matrix and its inverse.

makeCacheMatrix <- function(X = matrix()) {
  
  ## INV is the name of the stored inverse of X, in this environment.
  ## INV is initialized to NULL and remains NULL as long as the setinverse function is not called.
  INV <- NULL
  
  ## The set function allows to reuse the enriched matrix object for another matrix Y.
  ## It resets the matrix value (X) and the inverse INV to default NULL in the environment.
  ## Note : the double arrow "<<-" assignment allows to modify the parent environment of "set",
  ## which is the same for all 4 functions.
  set <- function(Y) {
    X <<- Y
    INV <<- NULL
  }

  ## The get function allows access to the matrix itself
  get <- function() X

  ## The setinverse function stores the value of the inverse in the parent environment 
  ## Note : again use of the double arrow assignment.
  setinverse <- function(inverse) INV <<- inverse
  
  ## The getinverse function accesses the inverse value stored in the parent environment 
  getinverse <- function() INV

  ## Returns the list of the 4 functons necessary to use the enriched matrix object  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The cacheSolve function is the companion function of makeCacheMatrix(). 
## When first called with an enriched matrix object, it computes the inverse of the matrix 
## associated with the object, stores it in the enriched matrix object and returns it.
## The next calls to cacheSolve will only return the stored value of the inverse 
## (unless set function has been used, which also resets the inverse).
## Note : in the assignment all matrices are considered invertible.

cacheSolve <- function(X, ...) {

  ## Reads the value of inverse stored in the enriched object  
  inverse <- X$getinverse()

  ## If inverse is not NULL, it means it has already been computed, so we reuse it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  ## If inverse is NULL, it means we have to compute it now with the standard R function.
  data <- X$get()
  inverse <- solve(data, ...)

  ## and then store it in the enriched object.
  X$setinverse(inverse)

  inverse  
}
