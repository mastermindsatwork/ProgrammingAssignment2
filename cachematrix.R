## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function "makeCacheMatrix" creates a "matrix" object to cache inverse. 
# This function contains 4 functions: set, get, setinverse, getinverse
# get is a function that returns the vector x stored in the main function.
# set is a function that changes the vector stored in the main function.
# setinverse and getinverse are functions similar to set and get.
# setinverse doesn't inverse, but store value of the input in a variable m.
# getinverse return stored value input (m) value.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#Function "cacheSolve" computes the inverse of "matrix", input of cached inverse 
#and returned by makeCacheMatrix function. 
#If inverse was already calculated and matrix was not changed, in that case
#cachesolve should return inverse from the cache instead of calculating again.
#If the inverse has not been calculated, data gets the matrix stored with 


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
