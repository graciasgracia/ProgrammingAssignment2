## This contains two functions: makeCacheMatrix() and cacheSolve.
## The first function creates a special "matrix" object that can
## cache its inverse. Th 2nd function computes the inverse of the
## special matrix returned by makeCacheMatrix

## makeCacheMatrix makes a list containing a function to:
## 1. set the value of the matrix
## 2. get the vaue of the matrix
## 3. set the value of the inverse of the matrix 
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
		Inv <- NULL
		set <- function(y){
			x <<- y
			Inv <<-NULL
		}
		get <-function() x
		setinv <- function(inv) Inv<<-inv
		getinv <- function() Inv
		list(set=set, get=get, setinv=setinv, getinv=getinv)
		
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then this function will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	Inv <- x$getinv()
	if(!is.null(Inv)){
		message("getting cached data")
			return(Inv)
	}

	d_matrix <- x$get()
	Inv <- solve(d_matrix)
	x$setinv(Inv)
	Inv			## Return a matrix that is the inverse of 'x'
}





        
