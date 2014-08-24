## The following functions create a special matrix container allowing to solve a matrix inverse and cache the result


## makeCacheMatrix creates a special matrix container containing the matrix and the cached result of its inverse.

makeCacheMatrix <- function(x = matrix())
{
    xinv <- NULL
    
    set <- function( y )
    {
        x <<- y
        xinv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function( inv ) xinv <<- inv
    
    getinverse <- function() xinv
    
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## cacheSolve solves the matrix inverse of a matrix created by makeCacheMatrix and store the result in the cache matrix.

cacheSolve <- function(x, ...)
{
    inv = x$getinverse()
    
    if( !is.null( inv ) )
    {
        message( "getting cached data" );
        return( inv )
    }
    
    data <- x$get()
    inv = solve( data, ... )
    x$setinverse( inv )
    inv
}
