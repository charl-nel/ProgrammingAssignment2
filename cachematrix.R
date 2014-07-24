#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#rather than computing it repeatedly . The functions below cache's the inverse of a matrix.

#This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.  It return a list 
#of functions for getting and setting the matrix and its inverse

makeCacheMatrix <- function(x = numeric()) {
        
        #make sure the cache is initially NULL
        im <- NULL
        
        #stores the matrix
        set <- function(y) {
                #matrix is cached to x
                x <<- y
                #clear the cache as matix is assigned to x
                im <<- NULL
        }
        
        #return the stored matrix x
        get <- function(){
              x 
                
        } 
        
        
        #cache the inverse of the matrix given
        setInverse <- function(solve){ 
                im <<- solve
        }
        
        
        #get the cached inverse of the matrix given
        getInverse <- function(){
                im
        }
        
        
        # Return the list that is actually a list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will 
#retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
         
                # get the cached matrix
                im <- x$getInverse()
                
                
                #if the cache is set return the value and cacheSolve is exited
                if(!is.null(im)) {
                        message("getting cached data")
                        return(im)
                }
                        
                # else if the cache is not set teh inverse is calculate and stores in cache
                data <- x$get()
        
                im <- solve(data, ...)
        
                x$setInverse(im)
                
                #return the Inverse Matrix
                im
}
