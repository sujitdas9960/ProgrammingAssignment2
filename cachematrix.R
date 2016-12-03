## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

matrix_val <- NULL
inverse_matrix <- NULL

## Function to create a square matrix
makeCacheMatrix <- function(x = matrix()) {
        if (all(is.na(x)))      {
                set.seed(10)
                x <- matrix(rnorm(16), nrow = 4, ncol = 4, byrow = TRUE)
        }
        matrix_val <<- x
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

        ## check if x has values and dimensions equal to matrix_val and also has inverse_matrix
        if (identical(matrix_val, x))   {
                if (is.null(inverse_matrix))
                        inverse_matrix <<- solve(x)
                else
                        message("getting cached inverse matrix")
                        
        }
        else
        {
                ## if not then get the inverse matrix by calling solve
                inverse_matrix <<- solve(x)
                
        }
        
        inverse_matrix
}
