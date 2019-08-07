#Funcion de Lucia Bernad:

#Celsius a Fahrenheit

#(0 °C × 9/5) + 32 = 32 °F

a <- c(0,1,10,20,100)

grados <- function(Far)
  
{Far= (a*(9/5) + 32)

return(paste(Far,"F"))

}

grados(a)


#Funcion de Bernardette Abadia:

#para calcular cuanta agua hay que agregar para acondicionar

#una masa de granos de peso p (gramos) desde una humedad inicial (hi, en%)

#hasta una humedad final hf (en %)



acond<-function(p,hf,hi)
  
{agregar <-p*(hf-hi)/(100-hf)

return(paste(agregar, "g de agua"))

}

acond(1000,16,13)

}




#Funcion de Leonardo Sallesses:

calc_vd <- function(dap, #diametro en metros
                    h   #altura en metros
){
  vd <-   3.14*((dap/2)**2)*h*0.42*600
  return(paste("La densidad volumétrica es de", round(vd,2), "Kg"))}

#Función de Lucas Bonelli:
##Pasar un muestreo de espigas/m a kg/ha 
#inputs: metros muestreados, ancho de hileras, 
#espigas obtenidas, peso seco medio de espiga)

rend_ha<- function(metros_lineales, dist_hileras, nro_espigas, peso_medio_esp){
  a<-metros_lineales #en metros
  b<-dist_hileras # en cm
  c<-nro_espigas #en la distancia lineal
  d<-peso_medio_esp # en gramos
  rha<-(c*d/1000)/(a*b/1000000)
  return(paste(round(rha, 2), "kg/ha"))
}

rend_ha(10, 52.5, 43, 197)