library(survival)
library(survminer)
#--------------------------------------------------
#Analisis de supervivencia
#--------------------------------------------------
###
# Funcion que prepara las trayectorias para analisis de supervivencia
# n es el numero de ventanas
supervivencia <- function(trayectorias, n){
  ventana_nac <- c()
  ventana_muerte <- c()
  sigue_viva <- c()
  for(i in 1:length(trayectorias)){
    ventana_nac <- c(ventana_nac, trayectorias[[i]][1])
    x <- trayectorias[[i]][1] + (length( trayectorias[[i]]) - 1) -1
    if(x >= n ){
      ventana_muerte <- c(ventana_muerte, NA)
      sigue_viva <- c(sigue_viva, TRUE)
    }else{
      ventana_muerte <- c(ventana_muerte, x)
      sigue_viva <- c(sigue_viva, FALSE)
    }
  }
  lista <- list(ventana_nac, ventana_muerte, sigue_viva )
  return(lista)
}

prueba10 <- supervivencia(t2, 215) 
### DATOS Con trayectorias de longitud >= 1
##Solo trayectorias no triviales ((mayores a 2))
superv2 <- data.frame(
  id = 1:length(t2),
  ventana_nac = prueba10[[1]],
  ventana_muerte = prueba10[[2]],
  sigue_viva = prueba10[[3]]
)

ULTIMA_VENTANA <- 215 #total de ventanas
#----
superv2$tiempo <- ifelse(
  superv2$sigue_viva,
  ULTIMA_VENTANA - superv2$ventana_nac + 1,
  superv2$ventana_muerte - superv2$ventana_nac + 1
)

###
superv2$evento <- ifelse(superv2$sigue_viva, 0, 1)

###
# Crear objeto Surv, para las trayectorias mayores a 2
superv_obj2 <- Surv(time = superv2$tiempo, event = superv2$evento)

# Ajustar Kaplan-Meier
km_fit2 <- survfit(superv_obj2 ~ 1, data = superv2)

# Ver resumen numérico
print(km_fit2)

ggsurvplot(km_fit2, 
           data = superv2,
           xlab = "Número de ventanas", 
           ylab = "Proporción de comunidades que sobreviven",
           title = "Curva de supervivencia de comunidades",
           surv.median.line = "hv",      # línea que marca la mediana
           risk.table = FALSE)            # tabla con el número en riesgo

###SUPERVIVENCIA DIVIDIDA POR PERIODOS
#---------------
superv2$periodo <- cut(
  superv2$ventana_nac,
  breaks = c(-Inf, 59, 73, 183,  Inf),
  labels = c("Pre-07/2019", "Entre 07/2019 y 02/2020", "Entre 02/2020 y 09/2024", "Post-09/2024" ),
  right = FALSE   # para que 60 caiga en el segundo grupo, ajusta según prefieras
)
table(superv2$periodo)
km_periodo <- survfit(Surv(tiempo, evento) ~ periodo, data = superv2)
print(km_periodo)

ggsurvplot(
  km_periodo,
  data = superv2,
  pval = TRUE,             
  conf.int = TRUE,          
  xlab = "Número de ventanas",
  ylab = "Proporción de comunidades que sobreviven",
  title = "Supervivencia de comunidades según época de nacimiento",
  legend.title = "Época de nacimiento",
  palette = c("steelblue", "orange", "tomato", "black")
)
