## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
  
makeCacheMatrix <- function(mat = matrix()) {    ## define function with arguments mat for matrix
        inv <- NULL                              ## initialize inv as null to hold inverse value
        set <- function(y) {                     ## define set function 
                mat <<- y                        ## set function gives new value of matrix in parent env  
                inv <<- NULL                     ## if new matrix exists, resets inv to null   
        }
        get <- function() mat                    ## define get - returns value of matrix 
        
        setinverse <- function(inverse) inv <<- solve(mat)  ## solves for inverse and caches it to parent env
        getinverse <- function() inv                        ## gets inv value from cache
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
        
}


## Write a short comment describing this function

## computes inverse of "mat" function from above function
## if inverse already exists for that specific matrix, then reads value from cache

cacheSolve <- function(mat, ...) {
        
        inv <- mat$getinverse() 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- mat$get()
        inv <- solve(data, ...)
        mat$setinverse(inv)
        inv
}
