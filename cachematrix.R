## These functions receive an invertible matrix and output the inverse of 
## that same matrix. They also store the matrix so the inverse does not have 
## to be recalculated everytime it is called

##This function takes a matrix as an input and 
##stores the original and its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinver<-function(solve) m<<-solve
	getinver<-function()m
	list(set=set, get=get,setinver=setinver,
		getinver=getinver)
}


##This function receives a matrix. Its job is to either calculate an inverse
##if there is non so far or to return the inverse that has already been stored

cacheSolve <- function(x, ...) {
	m<-x$getinver()
	if(!is.null(m)){
		message("Getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinver(m)
	m
}
