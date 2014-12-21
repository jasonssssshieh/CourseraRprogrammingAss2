## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

## the input for this function is an invertible matrix.
## For instance: mm<-matrix(runif(100),10,10)
## So, the right way to apply is: cm <-makeCacheMatrix(mm)

makeCacheMatrix <- function( m = matrix() ) {
        
        ## Initialize the inverse property
        i <- NULL
        
        ## Set the matrix
        setmatrix <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ##  Get the matrix
        getmatrix <- function() {
                ## Return the matrix
                m
        }
        
        ## Set the inverse of the matrix
        setInversematrix <- function(inverse) {
                i <<- inverse
        }
        
        ## Get the inverse of the matrix
        getInversematrix <- function() {
                ## Return the inverse property
                i
        }
        
        ## Return a list of the methods
        list(setmatrix = setmatrix, 
             getmatrix = getmatrix,
             setInversematrix = setInversematrix,
             getInversematrix = getInversematrix)
}

## The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setsolve`
# function.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInversematrix()
        
        ## Return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from the object
        data <- x$getmatrix()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data

        ## Set the inverse to the object
        x$setInversematrix(m)
        
        ## Return the matrix
        m
}
