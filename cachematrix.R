## Below are a pair of functions that cache the inverse of a square matrix.
## The makeCacheMatrix function is called initially to create a special "matrix".
## Then, the cacheSolve function can be called to returt the inverse.

## This function creates a special "matrix" object that can cache its inverse.
## Using the same steps as the example provided, the following steps are done:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix (inv_m)
## 4.  get the value of the inverse matrix (inv_m)
makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_m <<- solve
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve the
## inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        mm <- x$get()
        inv_m <- x$getinverse()        

        ## Check if the inverse has been calculated and return it if true.
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }                
        
        ## Calculate and cache the inverse if it has not been calculated
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinverse(inv_m)
        ## Return thr inverse
        inv_m
}
