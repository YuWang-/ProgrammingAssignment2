
##   makeCacheMatrix: This function creates a special "matrix" object that can 
##   cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##   `cacheSolve`: This function computes the inverse of the special
##   "matrix" returned by `makeCacheMatrix` above. If the inverse has
##   already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
# ## Sample run:
# > x = rbind(c(1, -2), c(-2, 1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1   -2
# [2,]   -2    1
# > cacheSolve(m)# No cache in the first run 
# [,1]       [,2]
# [1,] -0.3333333 -0.6666667
# [2,] -0.6666667 -0.3333333
# > cacheSolve(m)
# getting cached data #Retrieving from the cache in the second run
# [,1]       [,2]
# [1,] -0.3333333 -0.6666667
# [2,] -0.6666667 -0.3333333


