## this pair of functions caches the inverse of a matrix
## rather than compute the inverse of a matrix repeatedly

## The first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #always set i null to overwrite the previous inverse
        set <- function(y){ #this function sets a new value
                x <<- y
                i <<- NULL #always set i null to overwrite the previous inverse
        }
        get <- function() x #retrieves input matrix
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i #retrives the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() # inverse is retrieved from the cache if inverse has been calculated
        #if statement checks if i contains any matrix
        if(!is.null(i)){ 
                message("getting cached data")
                return(i)
        }
        matrix.data <-x$get()
        i <- solve(matrix.data,...)
        x$setinverse(i) #sets the inverse 
        i #returns i
}
