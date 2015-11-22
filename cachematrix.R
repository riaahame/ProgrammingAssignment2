## This R program contains definition and usage of two functions "makeCacheMatrix" and "cacheSolve" 
## Function makeCacheMatrix can be used to define a special matrix object that
##          can cache its inverse. It uses function setInverse to set the cache and 
##          getInverse to retrieve the cache value.
## Function cacheSolve is used to get the inverse of the matrix. It returns the value
##          from the cache if available. If not, it would calculate the inverse and 
##          store in cache and return the inverse.
         

## This function creates a "matrix" object that can cache its inverse 
## It defines couple of other functions such as setInverse & getInverse
## setInverse function is used to set the value to the cache
## getInverse is used to get the cache value
makeCacheMatrix <- function(x = matrix()) {

        Inv <- NULL
        set <- function(mat) {
                x <<- mat
                Inv <<- NULL
        }
        get <- function () x
        setInverse <- function(inverse) Inv <<- inverse
        getInverse <- function() Inv 
        list (set = set, get = get, setInverse = setInverse,
              getInverse = getInverse)
}


## This function gets the inverse of the matrix from the cache if its available in cache.
## If not, the inverse is calculated using the "solve" function and the cache is updated.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #First check in cache
        Inv <- x$getInverse()
        if (!is.null(Inv)) {
                #Inverse available in cache
                print("Getting cached data for Inverse...")
                return(Inv)
        }
        
        #Inverse not available in cache
        #Calculate inverse using "solve" function
        
        data <-x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}
