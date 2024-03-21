## Put comments here that give an overall description of what your
## functions do

## Cette fonction crée un objet « matrice » spécial 
## qui peut mettre en cache son inverse.

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## Cette fonction calcule l'inverse de la « matrice » spéciale 
## renvoyée par makeCacheMatrix ci-dessus. Si l'inverse a déjà été calculé

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}
