## These two functions are used to store and retreive inverse values of matrix x

## This function creates a list object that containes cached value of inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinver <- function(solve) inver <<- solve
    getinver <- function() inver
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
}


## This function retreives a cached value of inverse of matrix x, if avilable. If not, the function computes the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinver()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data,...)
    x$setinver(inver)
    inver
}
