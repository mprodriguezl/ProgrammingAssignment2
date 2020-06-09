## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function, makeCacheMatrix, creates a special "matrix", 
##which contains a list of 4 functions, including getting and
##and setting the inverse of an original matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve_matrix) inv <<- solve_matrix
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
##This second function, cacheSolve, calculates the inverse of
##the special "matrix". If the inverse matrix has already been
##calculated, it gets the inverse from the cache and skips the
##computation. If the inverse hasn't been calculated, then it 
##proceeds to calculate it and set the value of the inverse in 
##the cache with the set_inverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inverse(inv)
    inv
}
