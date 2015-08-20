#Calculate Inverse of a given matrix. Use cached data where possible

# This function creates a special "matrix" object that can cache its inverse.
# Using the example of makeVector() function in assignment question, 
# this function contains functions to get and set a matrix and also to get and set its inverse



makeCacheMatrix <- function(x = matrix()) {
  #Initialize inverse to NULL
  minv <- NULL
  #Take in matrix and set it to working matrix
  #Initialize inverse to NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  #Return working matrix
  get <- function() x
  
  #Take in object and set it to inverse of working matrix
  setinv <- function(inv) minv <<- inv
  
  #Return last calculated, i.e. CACHED inverse of working matrix
  getinv <- function() minv
  
  #Create handles for sub-functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Try to retrieve pre-calculated inverse
  i <- x$getinv()
  
  #If something was previously calculated and cached, it will be returned
  if(!is.null(i)) {
    
    message("getting cached data")
    #return cached data
    return(i)
  }
  
  #If nothing was returned, that means inverse has not been previously cached
  
  
  data <- x$get()
  #Solve for inverse
  i <- solve(data,...)
  
  #Cache for future use
  x$setinv(i)
  
  #Return inverse
  i
  
}
