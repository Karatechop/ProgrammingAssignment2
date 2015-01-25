# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	# intial value of inverse matrix 
	inv <- NULL
    
	# set matrix value
	set <- function(y) {
		x <<- y
		inv <<- NULL
		}
    
       	 # get matrix value
       	 get <- function() x
       	 
       	 # set inverse matrix value
       	 setinverse <- function(inverse) inv <<- inverse
    
       	 # get inverse matrix value
       	 getinverse <- function() inv
       	 
       	 # return a named list of all functions declared above
       	 list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the matrix 
## created with the makeCacheMatrix function. 
## It first checks to see if the inverse matrix already exists. 
## If it is there, it returns the inverse value directly from cache. 
## Otherwise, it calculates the inverse matrix value 
## and sets the value of the inverse in the cache by 
## saving it in an instance of setinverse function's environnment.

cacheSolve <- function(x, ...) {
	# test if there is an inverse matrix value in cache
	# if it is there, return inverse matrix value from cache
	inv <- x$getinverse()
	if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
        }
        
        # if inverse matrix value is not in cache, get the matrix
        data <- x$get()
        
        # calculate the inverse matrix value
        inv <- solve(data, ...)
        
        # cache the inverse matrix value 
        # (save it in an instance of setinverse environnment)
        x$setinverse(inv)
        
        # return the inverse matrix value
        inv
}
