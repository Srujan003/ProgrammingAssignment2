## This script employs two functions that help to save the costly computation of
## computing the inverse of a matrix. Together, they help in retrieving the previously
## calculated inverse from the cache if found again.

## This function creates a matrix object than has the ability to cache its inverse

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(y)          ## to set the matrix
  {
    a <<- b
    inv <<- NULL
  }
  get <- function(){a}        ## to get the matrix
  setInverse <- function(inverse){inv<<-inverse}                    ## to set inverse matrix
  getInverse <- function(){inv}                                     ## to get inverse matrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}

## The below function calculates the inverse of a matrix returned from the above function
## If it has been calculated already, the inverse is returned from cache.

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- a$getInverse()
  if(!is.null(inv))           ##checking if already present or not
  {
    message("Getting cached data")
    return (inv)
  }
  mat <- a$get()
  inv <- solve(mat,...)
  a$setInverse(inv)
  inv
}
