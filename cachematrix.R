##Loading these fuctions and passing matricies to the cacheSolve function
##will compute the inverse of the matrix. To reduce unneccessary computing,
##if the inverse has been computed, it will be retrevied from the cache.


##Use with CacheSolve() to add matrix inverse cache functionality
makeCacheMatrix <- function(x=matrix()) {
	i <- NULL	#initalize i as NULL meaning no cached value
	
	set <- function(y){		#stores matrix for caching 
		x <<- y
		i <<- Null
	}
	get <- function () 		#allows user to get original matrix
		x
	setinv <- function(inv) #passes inverse to the cache i
		i<<-inv
	getinv <- function() 	#returns inverse matrix 
		i
	
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
#Using with makeChacheMatrix() will eliminate the 
#need to recalculate the inverse matrix for an 
#input matrix that has just been computed.

	i <- x$getinv()		#passes either cached matrix or NULL into i
	
	#Determines if the inverse has already been
	#calculated by checking if i is NULL
	if (is.null(i)) {
			data <- x$get()
			i <- solve(data, ...)	#calculate inverse matrix
			x$setinv(i)		#store calculated value in i
	} else { print("Retrieving inverse matrix from cache") #notify user
		}
	i	#return inverse matrix
	
}
