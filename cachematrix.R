## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix takes a matrix as an argument and creates and object to
##      store the matrix along with its inverse. The inverse is not stored 
##      until cacheSolve is called on the returned object.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL

    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(get = get, setinv = setinv, getinv = getinv)
    
} 

## cacheSolve takes a makeCacheMatrix object as an argument and returns the 
##      inverse of the matrix stored in the makeCacheMatrix object. The inverse
##      is calculated the first time cacheSolve is called on the object, which
##      may be slow if the matrix is large. Subsequent calls on the same object
##      return the cached inverse matrix. 
cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    print("calculating matrix inverse")
    matrix_data <- x$get()
    inv <- solve(matrix_data)
    x$setinv(inv)
    inv
    
}
