makeCacheMatrix <- function(x = matrix()) { ## This function creates a special "matrix" object that can cache its inverse.
    invmat <- NULL ## this provides a default if makeCacheMatrix has never been used and sets invmat to null
    set <- function(y) { ## sets the value of the matrix
        x <<- y ## Caches the value of the matrix in the parent function
        invmat <<- NULL ## sets again by default the inverse of the matrix to null (to be filled later)
    }
    get <- function() x ## allow to get the matrix pre-defined
    setinverse <- function(inverse) invmat <<- inverse ## allows us to cache the inverse of the matrix if we know it so that cacheSolve doesn't waste time recalculating
    getinverse <- function() invmat ## allows us to get the inverse pre-defined
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) { ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
    invmat <- x$getinverse() ## calls the function previously defined to get the inverse of the matrix
    if(!is.null(invmat)) { ## if invmat is not null (if it has been filed before: cached) then: 
        message("getting cached data.")
        return(invmat) ## returns a message and the inverse of the matrix invmat
    }
    mat <- x$get() ## if not we get the matrix
    invmat <- solve(mat)## solve it
    x$setinverse(invmat) ## and fill invmat
    invmat ## then return it
}