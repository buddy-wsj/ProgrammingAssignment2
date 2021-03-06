##  Below are a pair of functions that are used to create a special object that stores 
##  a square matrix and caches its inverse.


## The first function, makeCacheMatrix, creates a special "matrix", which is really 
## a list containing methods to
##  set the matrix
##  get the matrix
##  set the inverse of the matrix
##  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        set_inverseMatrix <- function(inverseMatrix) im <<- inverseMatrix
        get_inverseMatrix <- function() im
        list(set = set, get = get,
             set_inverseMatrix = set_inverseMatrix,
             get_inverseMatrix = get_inverseMatrix)                

}


##  The following function calculates the inverse of the special "matrix" created  
##  with the above function. It first checks to see if the inverse has already 
##  been calculated. If so, it gets the inverse from the cache. Otherwise, 
##  it calculates the inverse and sets the inverse in the cache via the
##  set_inverseMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$get_inverseMatrix()
        if(!is.null(im)) {
                message("getting cached inverseMatrix")
                return(im)
        }        
        m <- x$get()
        im <- solve(m)
        x$set_inverseMatrix(im)
        im        
}
