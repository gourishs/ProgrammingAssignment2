## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                                          ##setting local inverse variable to NULL
        set <- function(y) {                                     ## Function to set new matrix
                x <<- y
                inverse <<- NULL                                 ##setting cache inverse variable to NULL - because new matrix
        }
        
        get <- function() x
        setinverse <- function(inverse) inverse <<-inv           ##setting cache inverse to calculated value 
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##returing list
}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- a$getinverse()
        if (!is.null(inverse)){                                 ##if cache inverse is not NULL, return the value
                message("getting cached data")
                return (inverse)
        }
        matrix <- a$get()
        inverse <- solve(matrix)                                ##Solve() function calculates inverse of matrix
        a$setinverse(inverse)
        inverse
}
