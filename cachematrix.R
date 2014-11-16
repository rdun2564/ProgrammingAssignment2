## This code is creates 2 functions which together allow the determination of the inverse 
## of a square matrix and to then store the returned inverse matrix to a cache variable
## so that it can be retrieved at a later time without the need to recalculate it.
## The code is written under the constraint that the matrix has a defined determinant.
##
## Instructions for using this code: 
## Step 1: Define the matrix to be inverted, eg a<- matrix(c(5,3,2,1), rnow=2, ncol=2)
## Step 2: Run the Function "makeCacheMatrix" using the matrix 'a' as its argument,
## the result from running makeCacheMatrix needs to be allocated to a variable:
##      For example: b <- makeCacheMatrix(a).
## Step 3: Run the Function cacheSolve using the result from makeCacheMatrix as its argument.
##      For example: for cacheSolve(b), returns inverted matrix(c(-1,3,2,-5), nrow = 2, ncol = 2)


## The makeCacheMatrix function uses the matrix required to be inversed as the argument.
## This function creates a special vector, which is really a list containing a function to:
## 1. set the value of the matrix, 2. get the value matrix, 3. set the value of inverse 
## 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ##  Note: the set function is not required for this code to work
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve function uses the result from running the makeCacheMatrix function as 
## argument. cacheSolve calculates the inverse of the matrix that was passed into makeCacheMatrix.
## However, it first checks to see if the inverse of the matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.  Otherwise, it calculates
## the inverse of the matrix and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
