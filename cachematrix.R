## Solving/Inverting matrix with less computational effort.  

## This function receive as input a matrix and outputs a special matrix to be 
## used by cacheSolve(). 


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
	set<-function(y){
	x<<-y
	m<<-NULL
	}
	get<-function() x
	setinv<-function(inv) {
	m<<-inv
	}
	getinv<-function() m
	list(set=set,get=get,
	setinv=setinv,
	getinv=getinv)
}



## This function inverts a special matrix stored by makeCacheMatrix() or, if 
## this function already has the result, cacheSolve() brings the value 
## previously stored in cache.

cacheSolve <- function(x, ...) {
    m<-x$getinv()
	if(!is.null(m)){
	message("getting cached data")
	return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinv(m)
	m 		## Return a matrix that is the inverse of 'x'
}
        
## Have a nice day! :) 