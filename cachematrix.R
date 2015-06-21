## Creates a matrix that can cache its inverse and a function that leverages
## the cacheable matrix to get the cached version or solve the inverse and
## cache it

## Creates a cacheable matrix from an existing matrix (or an empty one if not
## passed in

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
		x <<- y
                m <<- NULL
	}
        get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}


## Uses the cacheable matrix to return a cached inverse or calculate and set if
## it has not been cached before

cacheSolve <- function(x, ...) {
	m<-x$getsolve()
	if(!is.null(m)){
		message("Getting cached data.")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setsolve(m)
	m
}

