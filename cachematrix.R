#Please follow the following to use this function 

# First Create a matrix x using any method, for example
# > x <- matrix(c(1,4,5,7,3,5,2,9,8), nrow = 3)  
# Create Cache matrix
# > cachex <- makeCacheMatrix(x)
# get the vale
# > cachex$get()                                  
# checking for first time
# > cacheSolve(cachex) 
# checking for second time
# > cacheSolve(cachex)                            


makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inverse <- NULL
    
    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Getter for the matrix
    get <- function() x
    
    # Setter for the inverse
    setinv <- function(inv) inverse <<- inv
    # Getter for the inverse
    getinv <- function() inverse
    
    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    
    # If the inverse is already calculated, return it
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # The inverse is not yet calculated, so we calculate it
    message("Calculating for first time")
    data <- x$get()
    inverse <- solve(data, ...)
    
    # Cache the inverse
    x$setinv(inverse)
    
    # Return it
    inverse
}