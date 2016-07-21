## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # input x: a square invertible matrix
        # It will return a list containing functions :
        #              1. set the matrix
        #              2. get the matrix
        #              3. set the inverse
        #              4. get the inverse
        # This list is used as the input to cacheSolve()
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # return: inverse of the original matrix input to makeCacheMatrix()
        
        inv <- x$getinverse()
        
        # if inverse has already been calculated, cache value will be returned.
        if(!is.null(inv)) {
                print("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        
        # set the value of the inverse in the cache via the setinverse function.
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}



