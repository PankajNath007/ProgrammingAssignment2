## The cacheSolve function calculates the inverse of the special "matrix" created with makeCacheMatrix function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data (matrix) and sets the value of the inverse in the cache via the setInverse function


##  makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {


       inv_value <- NULL

       set <- function(y) {
    	
		x <<- y
		inv_value <<- NULL


       }  ## end set
   
    
       get <- function() x
       setInverse <- function(solve_value) inv_value <<- solve_value
       getInverse <- function() inv_value
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)






} ## end makeCacheMatrix



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        

	inv_value <- x$getInverse() 

	    if(!is.null(inv_value)) {
	            
		    message("getting cached data")
		    return(inv_value)
	
	    } ## end if

	data <- x$get()
	solve_value<- solve(data, ...)
	x$setInverse(solve_value)
	solve_value





} ## end cacheSolve
