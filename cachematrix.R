## Functions to cache the inverse of a matrix.
## Assumes the matrix is invertible; no error checking

## Creates a special "matrix" object able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NA  #default inverse matrix: NA
	
	#set matrix
	set <- function(y){
	    x <<- y
	    inv <<- NA  #reset inv to default
	}
	
	#get matrix
	get <- function() x
	
	#set inverse
	setinverse <- function(inverse) inv <<- inverse
	
	#get inverse
	getinverse <- function() inv
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes & returns inverse of special "matrix" x constructed by makeCacheMatrix.
## If inverse already computed and matrix has not changed, retrieves cached inverse
## else, calculates inverse and caches it for later recall.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
	
	#if inv == NA then dim(inv) == null...
	if(!is.null(dim(inv))){
	    message("getting cached inverse...")
		return(inv)
	}
	
	#else, calc inverse, cache it, then return val
	mat <- x$get()
	inverse <- solve(mat)
	x$setinverse(inverse)
	inverse 
}
