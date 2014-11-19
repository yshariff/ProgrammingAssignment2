## This R file contains two functions
## makeCacheMatrix function creates a special matrix which can be cached
## cacheSolve is a function which computes the inverse of a matrix only if its not present in the cache

## returns a cache-able matrix

makeCacheMatrix <- function(x = matrix()) {
	mat<-NULL
	set<-function(y){
	  x<<-y
	  mat<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) mat<<- solve
	getmatrix<-function() mat
	list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## returns the inverse of the matrix if present in cache, else computes inverse

cacheSolve <- function(x, ...) {
    	mat<-x$getmatrix()
    	if(!is.null(mat)){
      		message("Inverse already present, fetching from cache")
      		return(mat)
    	}
	message("Computing matrix inverse and caching")
    	matrix<-x$get()
    	mat<-solve(matrix, ...)
    	x$setmatrix(mat)
    	mat
}
