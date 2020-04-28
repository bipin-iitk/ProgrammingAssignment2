## This function will create a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()){
  
  inv <- NULL    ## Initialize the inverse property
  
       ## This function will set the matrix
        set <- function(y){
             x <<- y
             inv <<- NULL
        }
        
        ## This function is to get the matrix
        get <- function(){
            x                  ## Returning the matrix
        } 
        
        ## This is method to set the inverse of matrix
        set_inverse <- function(inverse){
          
             inv <<- inverse
        }

        ## Method to get the inverse of matrix
        get_inverse <- function(){ 
         
             inv        ## Returning the inverse property
        } 
        
  ## This will return the list of methods 
             
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function will calculate the inverse of matrix
## If already calculated, inverse will be obtained from cache data

cacheSolve <- function(x, ...){
     
  ## Return a matrix that is the inverse of x
        inv <- x$get_inverse()
        
        
        ## It will return the inverse from cache data if its already calculated
        
        if(!is.null(inv)){
              
              print("getting inverse of matrix from cached data")
              return(inv)
        }
        
        ## Obtainig the matrix data
        data <- x$get()
        
        ## ## Calculation of inverse of matrix
        inv <- solve(data, ...)
        
        ## Seting the inverse to the object
        x$set_inverse(inv)
        
        inv
}
