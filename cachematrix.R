## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(m){
        x <<- m
        inv_m <<- NULL        
    }
    get <- function() x 
    
    set_inv <- function(inv) inv_m <<- inv
    get_inv <- function() inv_m
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv_m <- x$get_inv()
    if(!is.null(inv_m)) {
        message("Getting cached data")
        return(inv_m)
    }
    m <- x$get()
    inv_m <- solve(m)
    x$set_inv(inv_m)
    inv_m
}