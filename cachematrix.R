
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## this function builds 
makeCacheMatrix <- function(x = matrix()) {
	## initial m to NULL
        m <- NULL
	## setup setter function, initialising x to y , m to null
	## note <<- searches through parent environments
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	## setup getter function
        get <- function() x
	## set solve m to solve
        setsolve <- function(solve) m <<- solve
	## get solve
        getsolve <- function() m
	## apply functions to this environment
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	## find out whether we have already solved for 'x'
        m <- x$getsolve()
	## excellent - we have not solved for x (m is not null)
        if(!is.null(m)) {
			## return cached value, print log message
                message("getting cached data")
                return(m)
        }
	## if we got here we haven't got a cached value, so calculate 
	## the inverse
        data <- x$get()
	## work out answer using solve
        m <- solve(data, ...)
	## cache the solution
        x$setsolve(m)
	## return the result
        m
}
