## Caching the Inverse of a Matrix
## Two functions:
## 1: create a cache matrix
## 2: cache the inverse of a matrix

## makeCacheMatrix create a matrix that can be inversed

makeCacheMatrix <- function(x = matrix()) {
inv_mat <-NULL
set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        set_inv_mat <- function(inverse) inv_mat <<- inverse
        get_inv_mat <- function() inv_mat
        list(set = set, get = get,
             set_inv_mat = set_inv_mat,
             get_inv_mat = get_inv_mat)
}


## CacheSolve function inverse the matrix and also retieve the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv_mat <-x$get_inv_mat()
if(!is.null(inv_mat))
{
return(inv_mat);
}
matrix_val<-x$get()
inv_mat<-solve(matrix_val, ...)
x$set_inv_mat(inv_mat)
inv_mat
}
