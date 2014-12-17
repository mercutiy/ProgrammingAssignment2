# Store the origin matrix and the inversed one
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the origin matrix
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    # Get the origin matrix
    get <- function() {
        x
    }
    
    # Set the inversed matrix
    set_inverse <- function(inv2) {
        inv <<- inv2
    }
    
    # Get the inversed matrix
    get_inverse <- function() {
        inv
    }
    
    list(
        set = set,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}


## Return cached inverse or calculate and store them 
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    m <- x$get()
    inv <- solve(m)
    x$set_inverse(inv)
    inv
}
