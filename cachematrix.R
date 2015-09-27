## See README.md for instructions on running the code and output from it
## makeCacheMatrix is a function that returns a list of functions
## Its puspose is to store a martix and a cached value of the inverse of the matrix .

# makeCacheMatrix creates a list containing a function to
# 1.set - set the value of the matrix
# 2.get -  get the value of the matrix
# 3.setMatrix -  set the value of inverse of the matrix
# 4.getMatrix -  get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     cinv<-NULL
     set <- function(y){
            x<<-y
            cinv<<-NULL
         }
     get<-function() x
     setMatrix <- function(solve) cinv<<-solve
     getMatrix <- function() cinv
     list(set=set , get=get , setMatrix=setMatrix , getMatrix=getMatrix )  

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
              cinv<-x$getMatrix()
              if(!is.null(cinv)) {
                message("getting cached data")
                return(cinv)
              }
              data<-x$get()
              cinv<-solve(data,...)
              x$setMatrix(cinv)
              cinv
        ## Returns a matrix that is the inverse of 'x'
}
