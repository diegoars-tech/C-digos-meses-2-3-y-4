calculatrayectorias <- function(umbral, listaBMJ, ventanas, umbral2){
  lista <- listaBMJ
  trayectorias <- list()
  for( i in  1: length(lista) ){
    for( j in 1:length(lista[[i]])){
      if(lista[[i]][[j]][1] != 0 && lista[[i]][[j]][3] > umbral ){
        x <- lista[[i]][[j]][2]
        c <- c(i, lista[[i]][[j]][1], x )
        if(i < length(lista)){
          for(k in (i+1): length(lista)){
            bandera <- TRUE
            for(l in 1: length(lista[[k]])){
              if(x == lista[[k]][[l]][1] && lista[[k]][[l]][3] > umbral ){
                comunidadOriginal <- ventanas[[i]][[j]]
                comunidadActual <- ventanas[[k]][[l]]
                jaccard <- length(intersect(comunidadOriginal, comunidadActual)) / length(union(comunidadOriginal, comunidadActual))
                if(jaccard > umbral2){
                  x <- lista[[k]][[l]][2]
                  c <- c(c, x)
                  lista[[k]][[l]] <- c(0,0,0)
                  bandera <- FALSE
                }
              }
            }
            if(bandera){
              break
            }
          }
        }
        trayectorias <- c(trayectorias, list(c))
      }
    }
  }
  return(trayectorias)
}
mistrayectorias <- calculatrayectorias(.5, prueba4, ventanas, .8)

t <- descarte(mistrayectorias, ventanas)

t2 <- seguimiento(t, 2)

