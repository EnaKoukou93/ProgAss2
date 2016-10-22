## These 2 functions create a special object that stores an invertible matrix 
## and caches its inverse, in order to avoiding wasting time with such costly
## computations.If the inverse of the matrix has already been calculated
## the inverse is not computed over and over again but the result is 
## retrieved from the cache. 

## Firstly, a special "matrix" object is created.This object  
## can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

	## x is a square invertible matrix
	
	s<-NULL
	setMatrix<-function(y){
		x<<-y
		s<<-NULL
	}
	getMatrix<-function() x
	setSolve<- function(solve) s<<-solve
	getSolve<-function() s

	## The function returns a list containing 4 finctions which will be used
	## as the input to the next function (cacheSolve())

	list(setMatrix=setMatrix,getMatrix=getMatrix,setSolve=setSolve,getSolve=getSolve)
}


## Finally,the inverse of the special "matrix" returned by the above function 
## is computed. If the inverse has already been calculated,then this function 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	## Returns the inverse of the original matrix input to makeCacheMatrix()
      
	s<-x$getSolve()

	## If the inverse has already been calculated get it from the cache and skip 
	## computation ,othewise, make the computation for the inverse

	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}
	data<-x$getMatrix()
	s<-solve(data,...)
	x$setSolve(s)
	return(s)
}
