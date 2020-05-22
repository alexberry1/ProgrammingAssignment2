makeCacheMatrix <- function(mat = matrix()) { 
        inv <- NULL                              
        set <- function(y) {                   
                mat <<- y                            
                inv <<- NULL                        
        }
        get <- function() mat                     
        
        setinverse <- function(inverse) inv <<- solve(mat)  
        getinverse <- function() inv                     
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
        
}



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