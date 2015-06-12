## Put comments here that give an overall description of what your
## functions do



## Comment 1: I created a clone Git directory on my desktop on 10 June 2015

## Comment 2: I added this edit comment to the file in the github fork on Thursday 11 June 2015

## Comment 3: Yet another comment on the 11 June 2015


## I added this edit comment to the file in the github fork on Thursday 11 June 2015


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        t <-NULL
        set <- function(y) {
                x <<- y
                t <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) t <<- inverse
        getinverse <- function() t
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        t <- x$getinverse()
        if (!is.null(t)) {
                message("getting cached data")
                return (t)
        }
        data <- x$get()
        t <- solve(data, ...)
        x$setinverse(t)
        t
}
