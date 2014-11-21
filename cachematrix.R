## This two functions/methods are calculating in inverse of a matrix, if the inverse isn´t already calculated.
## If it is, it returns the already calculated inverse from the cache.
## The first function...
##	make creates a special "matrix", which is really a list containing a function to
##	1.	set the value of the matrix
##	2.	get the value of the matrix
##	3.	set the value of the inverse
##	4.	get the value of the inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 ## create a matrix object x and some associated sub-functions/methods
		## set cache m zero.
        m <- NULL
        set <- function(y) {
                x <<- y 									## assign the input matrix y to the variable x in the
		m <<- NULL ## re-initialize m in the parent environment to null
        }
        get <- function() x ## set's the matrix x
        setinverse <- function(inverse) m <<- inverse ## set the cache m equal
        ## inverse the matrix
        getinverse <- function() m ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function...
## 1. tests if the inverse matrix exists. If it does, it skips the calculation.
## 2. If not, it calculates the value of the inverse and sends it into the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
