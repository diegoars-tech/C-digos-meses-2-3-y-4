# Este archivo genera en un archivo rds la informacion de una partición en
# comunidades
library(igraph)

df <- read.csv("Copia de NodosNames_fil_080.csv", stringsAsFactors = FALSE)

miLista  <- readRDS("Copia de vector_membresia_por_ventana_fil_080.rds")

# Funcion que separa en vectores cada comunidad de cada ventana
# el vector consta de los ids de los nodos que pertenecen a la comunidad, según
# el número de vector dentro de la lista es el numero de comunidad dentro de la
# red. Los ids son tomados del archivo NodosNames_fil_0X.csv
# La lista devuelta contiene x vectores con x el numero de comunidades 
# dentro de esa ventana

obtenComunidadesPorVentana <- function(obj, df){
  #Lista que de listas, cada sublista es una comunidad
  comunidadesVentana <- list()
  #obteniendo el numero total de comunidades por ventana
  x <-  length(unique(obj))
  ##Guardando cada comunidad en una lista en la lista grande
  for(j in 1:x){
    comunidad <- c()
    for(i in 1:length(obj)){
      if(obj[i] == j){
        for(k in 1: length(df[[1]])){
          if(is.na(names(obj[i]))){
            
          }else if( names(obj[i]) == df [k, 2]){
            comunidad <- c(comunidad, df[k, 1])
          }
        }
      }
    }
    comunidadesVentana[[j]] = comunidad
  }
  return(comunidadesVentana)
}
# Usamos la función para cada ventana de tiempo
comunidadesporventana <- list()
for(i in 1:215 ){
  comunidadesporventana[[i]] <- obtenComunidadesPorVentana(miLista[[i]], df)
}
# Generamos el archivo tipo rds
saveRDS(comunidadesporventana, file = "comunidadesporventana.rds")


