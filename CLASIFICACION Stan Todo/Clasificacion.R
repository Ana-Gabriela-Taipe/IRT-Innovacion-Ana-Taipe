library(openxlsx)
Informacion<-as.data.frame(read.xlsx("FuentesInformacion.xlsx"))
Financiamiento<-as.data.frame(read.xlsx("FuentesFinanciamiento.xlsx"))
Actividades<-as.data.frame(read.xlsx("Actividades.xlsx"))
Objetivos<-as.data.frame(read.xlsx("Objetivos.xlsx"))

Resultados<-as.data.frame(read.xlsx("Resultados.xlsx"))
Impactos<-as.data.frame(read.xlsx("Impactos.xlsx"))

suma_renglones <- function (x) {
  y=integer(nrow(x))
  for(i in 1:nrow(x)){
    for (j in 1:ncol(x)) {
      if(is.numeric(x[i,j])) 
      {
        y[i] = y[i]+(x[i,j])
      }
    }
  }
  x["Total"] <- y
  return(x)
}

#OBSERVACION: Capacidades = 1 (dividir para 4), Resultados = 1, Impactos= 1 

d<-cbind(Informacion,Financiamiento,Actividades,Objetivos)
d = suma_renglones(d)
Capacidades <- d$Total / 4

Capacidades[c(257,172,255,334,336)]
Resultados[c(257,172,255,334,336),1]
Impactos[c(257,172,255,334,336),1]



#------- Clasificacion ----------
IndiceHabilidad<-cbind(Capacidades, Resultados, Impactos)
IndiceHabilidad = suma_renglones(IndiceHabilidad)
IndiceHabilidad
IndHabilidad<-as.data.frame(IndiceHabilidad)
write_xlsx (IndHabilidad, 'Todo_08212022.xlsx' ) 


Indice_Total <- IndiceHabilidad$Total

PuestoC<-order(Capacidades, decreasing = FALSE)
PuestoR<-order(Resultados, decreasing = FALSE)
PuestoI<-order(Impactos, decreasing = FALSE)

#Obtenemo el número del puesto
Puesto<-order(Indice_Total, decreasing = FALSE)
#Obtenemos los valores del número del puesto
Valor_Indice<-Indice_Total[order(Indice_Total, decreasing = FALSE)]
#Unimos las dos columnas
Clasificacion<-cbind(Puesto,Valor_Indice,Capacidades,Resultados,Impactos)
 
#Extraemos la informacion en un archivo xlsx
library(writexl)
Orden<-as.data.frame(Clasificacion)
write_xlsx (Orden, 'OrdenStan_08212022.xlsx' ) 





