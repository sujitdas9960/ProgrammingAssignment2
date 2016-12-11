## Function returning list of functions handling matrix operations - set matrix,
## get matrix, set matrix inverse and get matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                ## check first if matrix has changed
                if (!identical(x, y))    {
                        x <<- y
                        inverse_matrix <<- NULL
                        message("assignment complete")
                }
                else    {
                        message("input matrix is same")
                }
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Return a matrix that is the inverse of input matrix. First checks cache
## before generating inverse matrix 
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        else    {
                message("inverse matrix is not cached")
        }
                
        matrix_val <- x$get()
        inverse <- solve(matrix_val, ...)
        x$setinverse(inverse)
        inverse
}

## Invoke functions for testing
## First create the matrix
## set.seed(10)
## mat <- matrix(rnorm(16), nrow = 4, ncol = 4, byrow = TRUE)

## get the list of functions
## matList <- makeCacheMatrix(mat)

## call cacheSolve first time. The function will print "inverse matrix is not
## in cache"
## cacheSolve(matList)

## call cacheSolve second time. The function will print "getting cached data"
## cacheSolve(matList)

## create a new matrix with different seed
## set.seed(25)
## mat <- matrix(rnorm(16), nrow = 4, ncol = 4, byrow = TRUE)

## set the new matrix. The function will print "assignment complete"
## matList$set(mat)

## call cacheSolve. The function will print "matrix inverse is not cached"
## cacheSolve(matList)

## set same matrix. The function will print "input matrix is same"
## matList$set(mat)

## call cacheSolve. The function will print "getting cached data"
## cacheSolve(matList)

