## The following code creates a special object of type makeCacheMatrix.
## This object returns a list with four elements, getters and setters for 
## a matrix which is passed as input upon instantiating an object of that type.

## Then a second function, cacheSolve, is introduced to cache the inverse of 
## that matrix and return it, if it's not already been calculated.


## This first function creates a cached matrix and returns a list of functions
## to get and set its values and the values of its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL 
    
    set <- function(y) { 
        x <<- y  
        i <<- NULL 
    }
    
    get <- function() x
    
    setinverse <- function (inverse) i <<- inverse 
    
    getinverse <- function () i 
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse) 
}


## This second function checks whether an inverse has already been solved
## for the matrix passed to makeCacheMatrix special object, if not it solves the
## inverse and caches its value in makeCacheMatrix special object. 

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    
    if (!is.null(i)) {
        message("getting cached inverse")
        return (i)
    }
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i
}

