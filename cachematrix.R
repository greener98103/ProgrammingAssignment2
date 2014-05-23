##This script holds two functions that cache and inverse a matrix
##makeCacheMatrix: This function creates a special "matrix" ##object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special ##"matrix" returned by makeCacheMatrix below. If the inverse has ##already been calculated (and the matrix has not changed), then ##the cachesolve should retrieve the inverse from the cache.
##############################################################
makeCacheMatrix <- function(x = matrix()) {
#create variable matrix and assign it a NULL value for now
             matrix_x <- NULL
        set <- function(x) { 
# Here is the set function that sets  a value into an object 
            matrix_x <<- matrix(x)
                x <<- matrix_x 
                matrix_x <<- NULL
        }
        get <- function() x
# Here is the set_inverse function that applies the solve application to a  matrix
# the get_inverse function creates a list in order to store the set and get values
        set_inverse <- function(solve) matrix_x <<- solve
        get_inverse <- function() matrix_x
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by #makeCacheMatrix above. 

cachesolve <- function(x) {
if(is.array(x) ){
         stop("This value has not been made into a special matrix object. Please run the makeCacheMatrix function first.")
}
if(is.null(x)  ){
         stop("This matrix is empty or not square.Try submitting a non-empty, square matrix")
}

#  If the special matrix has been loaded and inverse calculated then matrix_x will not be null.
# If the matrix has not changed, then the cachesolve 
# should retrieve the inverse from the cache.

        matrix_x <- x$get_inverse()
 
        if(!is.null(matrix_x)){
         message("checking special matrix object ... This matrix has already been inverted and cached. Retreiving...")
                return(matrix_x)
        }
          datax <- x$get()
# run get function
        matrix_x <- solve(datax)
# create inverse matrix
        x$set_inverse(matrix_x)
# set inverse matrix into specil object
        matrix_x
}
