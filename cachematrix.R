## Un par de funciones que almacenan en caché la inversa de una matriz


## Crea un objeto de matriz especial que puede almacenar en caché su inverso

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  
  ## Inicializa la propiedad inversa
  inv <- NULL
  
  ## Método para configurar la matriz
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Método para obtener la matriz
  ## Devuelve la matriz
  get <- function() {x}
  
  ## Método para establecer la inversa de la matriz
  setinverse <- function(inverse) {inv <<- inverse}
  
  ## Método para obtener la inversa de la matriz
  ## Devuelve la propiedad inversa
  getinverse <- function() {inv}
  
  ## Devuelve una lista de los métodos
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Obtener la caché de datos

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() 
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  
  ## Obtener la matriz de nuestro objeto
  mat <- x$get()
  
  ## Devuelve una matriz que es la inversa de 'x'
  inv <- solve(mat, ...)
  
  ## Establecer la inversa al objeto
  x$setinverse(inv)
  
  ## Devuelve la matriz
  inv
}
