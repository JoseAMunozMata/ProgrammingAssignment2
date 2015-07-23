## It is assumed that the matrix supplied as argument to makeCacheMatrix is always invertible.
## In case the matrix was singular -its determinant is equal to 0- R launches an error 
## indicating the singularity of the matrix passed as argument to the combination 
## of following the functions.

## The following functions calculate the inverse of an invertible matrix taking advantage 
## of the possibility to create and store the calculated inverse matrix in cache.

## In case the matrix changed from previouly inverted, the new inverse matrix is calculated
## and stored in cache.



## makeCacheMatrix function creates a special "matrix" object that can cache 
## the inverse of the matrix passed as argument in the function

makeCacheMatrix <- function(x = matrix()) {
	
	## Calculate and store the inverse of x in cache

	matriz <- x 
	set <- function(y) {
		x <<- y
		matriz <<- x
	}
	get <- function() x
	setinversa <- function(solve) matriz <<- solve(x)
	getinversa <- function() matriz
	list(set = set, get = get, 
		        setinversa = setinversa, 
		        getinversa = getinversa)
}



## cacheSolve function computes the inverse of the special "matrix" object returned by makeCacheMatrix above. 

## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve function retrieve the inverse from the cache.

## Otherwise, the inverse matrix of the one passed as argument 
## in makeCacheMatrix function is calculated and stored in cache.

cacheSolve <- function(x, ...) {
      
	## Return a matrix that is the inverse of x

	inversa <- x$getinversa()
	if(!is.null(inversa)) {
		message("getting cached data")
		inversa
	}
	data <- x$get()
	inversa <- solve(data)
	x$setinversa(inversa)
	inversa
}

