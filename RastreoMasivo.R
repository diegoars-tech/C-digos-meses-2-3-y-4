ventanas <- readRDS("comunidadesporventana.rds")
###
#funcion que devuelve el indice de Jaccard entre cada pareja de comunidades 
#de dos ventanas distintas
jaccardentreVentanas2 <- function(ventanat, ventanatmas1){
  indices <- list()
  y <- 1
  for(i in 1:length(ventanat)){
    for(j in 1:length((ventanatmas1))){
      x <- length(intersect(ventanat[[i]], ventanatmas1[[j]])) / length(union(ventanat[[i]], ventanatmas1[[j]]))
      v <- c(i, j, x)
      indices[[y]] <- v
      y <- y+1
    }
  }
  indices1 <- list()
  z <- 1
  for(i in 1: length(indices)){
    if(indices[[i]][3] != 0 ){
      indices1[[z]] <- indices[[i]]
      z <- z+1
    }
  }
  return (indices1)
}
#prueba <- jaccardentreVentanas2(ventanas[[107]], ventanas[[108]])

#Funcion auxiliar que separa las comparaciones de una comunidad.
#Devuelve una lista de listas. Cada sublista contiene  comparaciones de una 
#comunidad contra todas en t+1
separaComunidad <- function(indices){
  keys <- sapply(indices, function(v) v[1])    
  split(indices, keys)                          
}
#prueba2 <- separaComunidad(prueba)

###Funcion que identifica el indice más alto de una comunidad
#en t, con otra comunidad en t+1
#numcom son el numero de comunidades en la ventana t
bestMatchFiltro <- function(indicesSeparados){
  indicesAltos <- list()
  y <- 1
  for(i in 1:length(indicesSeparados)){
    x <- 0
    for(j in 1: length(indicesSeparados[[i]])){
      if(x < indicesSeparados[[i]][[j]][3]){
        x <- indicesSeparados[[i]][[j]][3]
        indicesAltos[[y]] <- indicesSeparados[[i]][[j]]
      }
    }
    y <- y+1
  }
  return(indicesAltos)
}
#prueba3 <- bestMatchFiltro(prueba2)

###
#Funcion que hace el calculo BestMatch para todas las ventanas
#Hace uso de las funciones anteriores para todas las comparaciones entre ventanas
jaccardbestMatchMasivo <- function(ventanas){
  jaccardscompletos <- list()
  for(i in 1:(length(ventanas)-1)){
    aux <- jaccardentreVentanas2(ventanas[[i]], ventanas[[i+1]])
    aux2 <- separaComunidad(aux)
    jaccardscompletos[[i]] <- bestMatchFiltro(aux2)
  }
  return(jaccardscompletos)
}
###------------------------
#Uso de funciones
jaccardstotales <- jaccardbestMatchMasivo(ventanas)
#Creamos el archivo para no correrlo cada vez
saveRDS(jaccardstotales, file = "jaccardstotales.rds")

