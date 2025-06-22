## makeCacheMatrix: creates a special “matrix” object that can cache its inverse
## returns a list of four functions to set/get the matrix and to set/get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              
  
  set <- function(y) {
    x <<- y                
    inv <<- NULL          
  }
  
  get <- function() x      
  
  setinverse <- function(inverse) inv <<- inverse  
  
  getinverse <- function() inv  
  
  list(set         = set,
       get         = get,
       setinverse  = setinverse,
       getinverse  = getinverse)
}

##  This function first checks whether an inverse is already stored; if so it returns that
## otherwise it computes the inverse with solve(), stores it, and returns it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()    
  if (!is.null(inv)) {     
    message("getting cached inverse")
    return(inv)            
  }
  
  mat <- x$get()           
  inv <- solve(mat, ...)   
  x$setinverse(inv)        
  inv                       
}
