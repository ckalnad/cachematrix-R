## makeCacheMatrix -> Create matrix object that can cache its inverse
## cacheSolve -> Compute inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- Null
	set <- function(y){
		x <<- y
		m <<- Null
}

get <- function()x
setinverse <- function(inverse)m <<- inverse
getinverse <- function()m
list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)){
		message('getting cached data')
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m

}