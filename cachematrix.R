########
### A set of functions creating an object of a matrix for caching the inverse matrix
### of the original matrix in the object
########

## Function to create an object of a matrix for caching the inverse
## Expecting a matrix as argument x, using an empty matrix as default 
## Containing the functions to "set" and "get" the matrix as well as set and get  
## the inverse matrix "set_inv" and "get_inv"

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL # Creating an empty variable to store the inverse matrix
    
    ## Function to create the object, intitalisting it with the matrix and setting 
    ## the inverse matrix to empty
    ## Expecting a matrix as argument "m"
    set <- function(m){
        x <<- m # Setting the variable x from the calling environment to the matrix m
        inv_m <<- NULL # Setting the variable inv_m from the calling enviroment to empty
    }
    
    ## Function to return the stored matrix (no parameters)
    get <- function() {
        x # Returns the stored matrix from variable x in the calling environment 
    }
    
    ## Function to set the inverse matrix in the object
    ## Expecting the inverse matrix as argument "inv"
    set_inv <- function(inv) {
        inv_m <<- inv # Setting the variable inv_m of the calling environment to the received value inv
    }
    
    ## Function to get the stored inverse matrix (no parameters)
    get_inv <- function() {
        inv_m # Returns the stored inverse matrix from variable inv_m in the calling environment 
    }
    
    ##  Creating and returning a list of the functions with the same names for the list to call the functions by name
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
    
} # End of makeCacheMatrix()


## Function to get the inverse matrix of a cached matrix x storing it in the object
## and treturning it form the object when already stored
## x has to be of type makeCacheMatrix

cacheSolve <- function(x) {
    
    inv_m <- x$get_inv() # Get the stored inverse matrix of x and assign it to variable inv_m (Returns null if the inverse was not stored yet)
    
    # When inv_m is not null return it 
    if(!is.null(inv_m)) { 
        message("Getting cached data") # Informing about cached data
        return(inv_m) # Return the inverse matrix and end function
    }
    
    ## This part is only executed when inv_m was not set yet
    
    m <- x$get() # Get the stored matrix assign it to m
    inv_m <- solve(m) # Calculate the inverse matrix of m and assign it to inv_m
    x$set_inv(inv_m) # Set the inverse matrix in the stored object to the calculated inverse for future references
    inv_m # return the inverse matrix
    
} # End of cacheSolve()