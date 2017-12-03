## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 
#' Title : Make Cached Matrix
#'
#' @param mat: the matrix to be cached, defaults to empty matrix
#'
#' @return list object with set/get methods for matrix, and get_inv/set_inv for inverse
#' @export
#'
#' @examples list_obj <- makeCacheMatrix(matrix(1:4, 2, 2))
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function (y){
      mat <<- y
      inv <<- NULL
    }
    
    get <- function () mat
    set_inv <- function (input) inv <<- input
    get_inv <- function () inv
    list(
        set=set,
        get=get,
        set_inv=set_inv,
        get_inv=get_inv
    )
}


## Write a short comment describing this function

#' Title: cache solve for matrix object
#'
#' @param x:  the list object returned by makeCacheMatrix
#' @param ... : extra args
#'
#' @return an inverse of the matrix, calculated, or retrieved from cached value
#' @export
#'
#' @examples : sample_inverse <- cacheSolve(makeCacheMatrix(matrix(1:4, 2, 2)))
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if (!is.null(inv)){
       message("getting cached inverse") 
       return(inv) 
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$set_inv(inv)
    inv
}
    