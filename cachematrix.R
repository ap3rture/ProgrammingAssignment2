## The following functions allow a user to set the value of a matrix, retrieve
## that matrix, compute the inverse of the matrix, and store and retrieve
## previously computed inverse values

## makeCacheMatrix is a function which allows the user to set the initial value
## of a matrix, retrieve that value, and get the value of the inverse of that 
## matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        get <- function() x  
        
        ##retrieve original matrix
        
        computeinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        
        ##compute inverse of original matrix 
        
        list(get = get, getinverse = getinverse, computeinverse=computeinverse)
        
        ##define usable list of functions for makeCacheMatrix
}


## cacheSolve computes, caches and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        
        ##set the inverse equal to the already computed data, if applicable
        
        if(!is.null(inverse)) {
                message("one moment...retrieving cached data")
                return(inverse)
        }
        
        ##if inverse has been previously computed, print message stating data
        ## is being retrieved and return the previously computed data
        
        data <- x$get()
        inverse <- solve(data, ...)
        inverse
        
        ##if inverse has not been previously computed, compute the inverse of
        ##the original matrix and print the result to the console
}
