##Programa para inverter matriz, referente à semana 3 do curso R programming do Coursera

## A funcao armazena a matriz, com ela podemos armazenar em cache para usar repetidamente.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
        get = get, setinverse = setinverse, getinverse = getinverse)
}

##A segunda função calcula a inversa da matriz retornada pela função makeCacheMatriz. Se a inversa já foi calculada e a matriz não mudou, então a função cacheSolve deve retornar a inversa armazenada na Cache

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)  
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
