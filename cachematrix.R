## Put comments here that give an overall description of what your
## functions do
## The functions follow closely the example given. Instead of processing a 
## vector and calculating the mean, a matrix is being inverted via the 'solve' 
## funtion. There are no checks done - as per instructions - to check if the 
## matix is invertible. remark for this part: the matrices constructed to test 
## the functions have been populated by random numbers and that gave never 
## problems.
##
## Write a short comment describing this function
## makeCacheMatrix returns an object that includes 4 functions
## to 
## - set values of the invertible matrix 
## - get the values of the matrix 
## - setinv - to calculate the inverse of the matrix
## - getinv - to get the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
## cachesolve takes advantage of the previously defined functions
## included in the object that needs to be passed to it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
##		if 'inv' is not null the inverted matrix is available in cache
##		from previous calculation		
                message("getting cached data")
                return(inv)
        }
## not cached: the matrix is being inverted by solve
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}