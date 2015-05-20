#This gets called first with the matrix in question
#to provide caching functionality.
#Usage...  "Magic = makeCacheMatrix(B)
#           cacheSolve(Magic)"

makeCacheMatrix <- function(originalMatrix = matrix()) { 
  #Clear out the existing inverse matrix, there's going to be a new one
  theInverseMatrix <- NULL 
  
  #Once the magic function list is created, the "set" function can be used
  #to change the original matrix
  set <- function(y) {
    originalMatrix <<- y
    theInverseMatrix <<- NULL
  }
  
  #This function can be used to retrieve the set of numbers
  get <- function() originalMatrix
  
  #Once calculated, this caches the mean for later
  setinverse <- function(givenInverse) theInverseMatrix <<- givenInverse
  
  #Retrieve the pre-calculated mean (null if not calculated yet)
  getinverse <- function() theInverseMatrix
  
  #Return the four functions, packaged as a list of four elements
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(MagicFunctionList, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Try retrieving a pre-calulated inverted matrix
  PreCalculatedMatrixInverse <- MagicFunctionList$getinverse()
  
  #If it's not null then return it, you're done!
  if(!is.null(PreCalculatedMatrixInverse)) {
    message("getting cached data")
    return(PreCalculatedMatrixInverse)
  }
  
  #If we got here, it must have been null.  Need to calculate it...
  #Grab the Matrix
  newMatrix <- MagicFunctionList$get()
  
  #Calculate the inverted matrix
  newMatrixInverse <- solve(newMatrix, ...)
  
  #Store it for later
  MagicFunctionList$setinverse(newMatrixInverse)
  
  #Return the newly calculated inverted matrix
  newMatrixInverse 
}
