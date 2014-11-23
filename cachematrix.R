## This script will call two functions when operating with invertible matrices
## First function, makeCacheMatrix(), will create a special 'matrix' object that can cache its inverse.
## Second function, cacheSolve(), will calculate the inverse of the special 'matrix' 
## returned by the first function. If the inverse has already been calculated (and the matrix has not changed), 
## then the second function should return the inverse from the cache.

## Calling makeCacheMatrix() function will create a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_mtx <- NULL  ## sets the inverse matrix to be NULL every time the makeCacheMatrix() function is called
        set <- function(y) { ## this fuction will allocate 'x' superassigned as 'y'
                x <<- y
                inv_mtx <<- NULL   ## superassigning the inverse matrix NULL
        }
        get <- function() x     ## This function will get the original matrix 'x' 
                                ## as an independant variable it will be called from local or global environment
        setinv <- function(inverse) inv_mtx <<- inverse  ## this is called by cacheSolve() function 
                                                         ## during the first run and
                                                ## it will store the value of inversed matrix using superassignment
        getinv <- function() inv_mtx   ## this will return the cached value to cacheSolve() function on subsequent accesses
        list(set = set, get = get,   ##  this is accessed each time makeCacheMatrix() function is called, 
             setinv = setinv,        ## each time we make a new object.
             getinv = getinv)        ## It's a list of the internal functions (or 'methods'), so when calling the 
                                     ## function, it will know how to access those methods.
}

## When cacheSolve() function is called it will see if the inverse of 'x' is stored.  
## If not, it will calculate the inverse matrix, store it and then return it.
## If the inversed matrix of 'x' has been calculated and stored before,
## it will fetch that inverse matrix and return it with the message 'getting cached data'.

cacheSolve <- function(x, ...) {        ## using 'x', this function uses the output of makeCacheMatrix()
                                        ## and returns a matrix that is the inverse of the original 'x'
        inv_mtx <- x$getinv()       ## accesses the object 'x' and gets the values of the inverse matrix
        if(!is.null(inv_mtx)) {         ## If inverse matrix has been calculated and is not NULL, the if loop will run
                message("getting cached data")  ## this will indicate that the inverse matrix was in the cache,
                                                ## sending a message to the console
                return(inv_mtx)      ## and returns the inverse matrix already calculated
        }
        mtx_data <- x$get()   ## we reach this code only if x$getinv() is returned NULL
        inv_mtx <- solve(mtx_data, ...)  ## if inverse matrix was NULL, then we have to solve it
        x$setinv(inv_mtx)    ## and store the calculated inverse matrix 
        inv_mtx              ## returns the inverse matrix to the code that called this function
}
