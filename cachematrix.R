## This is a function that cache the inverse of a matrix
## It is useful for saving time when dealing with big data frames
##
##

## the first function creates a special "matrix", which is a list
## containing a function to
##  set - set the value of the matrix
##  get - get the value of the matrix
##  setmatrix - set the value of the inverse matrix
##  getmatrix - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    mat<-NULL
    set<-function(y){
        x<<-y
        mat<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) mat<<- solve
    getmatrix<-function() mat
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## The second function calculates the inverse of the special matrix
## created with the prevoius function, using cached result if
## specified

cacheSolve <- function(x=matrix(), ...) {
    mat<-x$getmatrix()
    if(!is.null(mat)){
        message("getting cached data")
        return(mat)
    }
    matrix<-x$get
    mat<-solve(matrix, ...)
    x$setmatrix(mat)
    mat
}
