##if the contents of the matrix are not changing, it would be a efficient way to cache the inverse
##so that when we need it again, it can be looked up in the cache rather than recomputed


##this function returns a list, containing functions to
#set the value of the matrix
#get the value of the matrix
#set the value of inverse of the matrix
#get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    #returns a list containing functions to 1, set value of the matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##this function calculates the inverse of a matrix.
#it first check if the inverse has already been calculated. if so, it gets the inverse from cache and skips the calculation
#otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function
cacheSolve <- function(x, ...){
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        #return inverse that has been calculated and stored in the cache
        return (inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    #return a matrix that is the inverse if x
    inv
}