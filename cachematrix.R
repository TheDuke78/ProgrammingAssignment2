## Function: makeCacheMatrix 
## Author: Richard vd Pol
## Date: 23-7-2015
## Desription: 
## Constructor for creating a cacheMatrix object 
## Parameters: a matrix object.
## Output an object with four methods:
## Set : sets the object in a different scope and clears the cache
## Get : Returns the given matrix
## SetMatrix: saves the solved matrix in a different scope
## getmatrix: retrieves the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## Function: cacheSolve 
## Author: Richard vd Pol
## Date: 23-7-2015
## Desription: 
## Function is called with parameter X of type makeCacheMatrix
## Parameters: an object of makecachematrix
## Output: a inverse of the input matrix object. If it is calculated it saves 
## the result is a different scope. If it was previously calulated it retrieves the 
## cached version.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached matrix")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
