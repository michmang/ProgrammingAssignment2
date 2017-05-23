#The two functions below are built to cache the inverse of a matrix.
# Since investion is usually a costly computation and it is better
# to caching the inverse of a matrix rather than compute it over and over again. 
# makeCacheMatrix is creating a list containing a function to
# 1. set the value of the matrix using the set
# 2. get the value of the matrix using the get
# 3. set the value of inverse of the matrix using the setinverse
# 4. get the value of inverse of the matrix using the getinverse

makeCacheMatrix <- function(x = matrix()){
        INV <-NULL
        set <- function(y) {
                x <<- y
                INV <<- NULL   
   }
            
        get <- function()x 
        setinverse <-function(inverse)INV <<-inverse
        getinverse <- function() INV
        list (set=set, get= get,
              setinverse = setinverse, getinverse = getinverse)
}



## Function cacheSolve returns the inverse of the matrix. The steps are:
## Checks if the inverse has already been computed. 
## If yes, it gives a message and gets the result. 
## If no, it computes the inverse, sets the value in the cache via setinverse function.
# This function is assuming that the matrix has been invertible

cacheSolve <- function(x, ...) {
        INV <- x$getinverse()
        if(!is.null(INV)) {
                message("caching data.")
                return(INV)
        }
        data <- x$get()
        INV <- solve(data)
        x$setinverse(INV)
        INV
}
