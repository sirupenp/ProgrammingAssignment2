###############################Function Definitions############################
# makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse,
# these functions are used:
# ## setMatrix() sets the value of matrix 
# ## getMatrix() gets the value of matrix
# ## setInvMatrix() sets the value of inverse matrix
# ## getInvMatrix() gets the value of inverse matrix


 makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL  
    setMatrix <- function(y) {    
    x <<- y    
    m <<- NULL  
    }
	
#Get the value of matrix from cache  
 getMatrix <- function() x
	
#Set the value of inverse matrix into cache  
 setInvMatrix <- function(inverseSet) m <<- inverseSet
	
#Get the value of inverse matrix into cache  
 getInvMatrix <- function() m
	 
	 
#Output   
 list(setMatrix = setMatrix, getMatrix = getMatrix, 
     setInvMatrix = setInvMatrix,       
     getInvMatrix = getInvMatrix)
}

# cacheSolve is a function that accepts function list from makeCacheMatrix as
# parameter and checks if the inverse of the matrix already exits.
# It then returns the data from cache if it exists else it compute using "solve" function and
# caches using function setInvMatrix() defined in the makeCacheMatrix()
	
cacheSolve <- function(x, ...) {  
    m <- x$getInvMatrix()  
  #Check to see if the matrix-inverse already exists using getInvMatrix(), if it does then return cached data  
    if(!is.null(m)) {    
    message("getting cached data")    
    return(m)  
    }
	
#If the Inverse doesn't exist in cache then compute and store in cache using the setInvMatrix()  
  data <- x$getMatrix()  
  m <- solve(data, ...)  
  x$setInvMatrix(m)  
  m
	
}
