## makeCacheMatrix takes an matrix and creates an object from which you
## can get the matrix back but also the inverse of the matrix if it has
## been computed and then by calling the cacheSolve you can compute the
## inverse of the matrix or take it from the cache if it was already
## computed

## This function creates the special caching object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setData <- function (y)
    {
        x <<- y
        inv <<- NULL
    }
    getData <- function () x
    setInv <- function (invMat) inv <<- invMat
    getInv <- function () inv
    list (setData = setData, getData = getData, setInv = setInv, getInv = getInv)
}


## This function will get you the inverse of a matrix but if that
## was already computed then the computation is skipped and you get
## the cached result

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv))
    {
        message("getting cached inverse")
        return(inv)
    }
    # if nothing in cache then compute
    data <- x$getData()
    inv <- solve(data)
    x$setInv(inv)
    x$getInv()
}
