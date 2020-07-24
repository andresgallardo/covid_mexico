### SCRIPT PARA HACER TABLAS DE VIDA MEXICO Y ESCENARIOS
## NOTAS: cargar covid_mort del script: 1_cargar_datos (datos de poblacion y mortalidad
##        de proyecciones de conapo y de numero de muertes de secretaria de salud)
## autor: andrés gallardo

# library(tidyr)
# library(data.table)
# library(LifeTable)
# library(dplyr)
# library(writexl)

#PREPARAR DATAFRAME t DE 1_cargar_datos
#t<-t

#TABLAS DE VIDA Y EXTRAER E0
  #crear tablas por sexo
  tf<-filter(t,sexo==1) #mujeres (f)
  tm<-filter(t,sexo==2) #hombres (m)

  #preparar el loop para hacer la lista por estado
  estados <- unique(t$entidad)

#####ESCENARIO BASE: conapo: muertes
  #crear vectores vacios
  expf<-vector("list",length(estados))
  expm<-vector("list",length(estados))

  #hacer loop por estado para cada sexo
  for (i in estados) {
    tf_i<-filter(tf,entidad==i)
    lt5f_i<-LT(tf_i$poblacion,tf_i$muertes, ages=c(0,1,seq(5,100,by=5)),sex = "female", axmethod = "preston", mxsmooth =FALSE, axsmooth = TRUE)
    expf[[i]]<-lt5f_i$ex
  }
  for (i in estados) {
    tm_i<-filter(tm,entidad==i)
    lt5m_i<-LT(tm_i$poblacion,tm_i$muertes, ages=c(0,1,seq(5,100,by=5)),sex = "male", axmethod = "preston", mxsmooth = FALSE, axsmooth = TRUE)
    expm[[i]]<-lt5m_i$ex
  }
  #agregar numero de entidad a cada vector
  names(expf) <-estados
  names(expm) <-estados
  #convertir listas a dataframe
  #mujeres
  exf<-as.data.frame(expf)
  exf<-exf[1,] #seleccionar e0
  exf<- gather(exf,X)
  exf<-exf%>%select(.,-c(X))
  names(exf)<-("e0_f_proyecciones")
  #hombres
  exm<-as.data.frame(expm)
  exm<-exm[1,]
  exm<- gather(exm,X)
  exm<-exm%>%select(.,-c(X))
  names(exm)<-("e0_m_proyecciones")
  #hacer dataframe final
  e0_proyecciones<-cbind(exf,exm)

#####ESCENARIO BAJO: covid secretaria de salud: muertes_totales
  #crear vectores vacios
  excf<-vector("list",length(estados))
  excm<-vector("list",length(estados))

  #hacer loop por estado para cada sexo
  for (i in estados) {
    tf_i<-filter(tf,entidad==i)
    lt5f_i<-LT(tf_i$poblacion,tf_i$muertes_totales, ages=c(0,1,seq(5,100,by=5)),sex = "female", axmethod = "preston", mxsmooth = FALSE, axsmooth = TRUE)
    excf[[i]]<-lt5f_i$ex
  }
  for (i in estados) {
    tm_i<-filter(tm,entidad==i)
    lt5m_i<-LT(tm_i$poblacion,tm_i$muertes_totales, ages=c(0,1,seq(5,100,by=5)),sex = "male", axmethod = "preston", mxsmooth = FALSE, axsmooth = TRUE)
    excm[[i]]<-lt5m_i$ex
  }
  #agregar numero de entidad a cada vector
  names(excf) <-estados
  names(excm) <-estados
  #convertir listas a dataframe
  #mujeres
  exf<-as.data.frame(excf)
  exf<-exf[1,] #seleccionar e0
  exf<- gather(exf,X)
  exf<-exf%>%select(.,-c(X))
  names(exf)<-("e0_f_covid")
  #hombres
  exm<-as.data.frame(excm)
  exm<-exm[1,]
  exm<- gather(exm,X)
  exm<-exm%>%select(.,-c(X))
  names(exm)<-("e0_m_covid")
  #hacer dataframe final
  e0_covid<-cbind(exf,exm)

#ESCENARIO MEDIO: ihme medio con la misma proporcion por edad: muertes + muertes_nov
  #crear vectores vacios
  exih2f<-vector("list",length(estados))
  exih2m<-vector("list",length(estados))

  #hacer loop por estado para cada sexo
  for (i in estados) {
    tf_i<-filter(tf,entidad==i)
    lt5f_i<-LT(tf_i$poblacion,tf_i$muertes+tf_i$muertes_nov*(tf_i$covid/tf_i$tot), ages=c(0,1,seq(5,100,by=5)),sex = "female", axmethod = "preston", mxsmooth = FALSE, axsmooth = TRUE)
    exih2f[[i]]<-lt5f_i$ex
  }
  
  for (i in estados) {
    tm_i<-filter(tm,entidad==i)
    lt5m_i<-LT(tm_i$poblacion,tm_i$muertes+tm_i$muertes_nov*(tm_i$covid/tm_i$tot), ages=c(0,1,seq(5,100,by=5)),sex = "male", axmethod = "preston", mxsmooth = FALSE, axsmooth = TRUE)
    exih2m[[i]]<-lt5m_i$ex
  }
  #agregar numero de entidad a cada vector
  names(exih2f) <-estados
  names(exih2m) <-estados
  #convertir listas a dataframe
  #mujeres
  exf<-as.data.frame(exih2f)
  exf<-exf[1,] #seleccionar e0
  exf<- gather(exf,X)
  exf<-exf%>%select(.,-c(X))
  names(exf)<-("e0_f_ihme")
  #hombres
  exm<-as.data.frame(exih2m)
  exm<-exm[1,]
  exm<- gather(exm,X)
  exm<-exm%>%select(.,-c(X))
  names(exm)<-("e0_m_ihme")
  #hacer dataframe final
  e0_ihme<-cbind(exf,exm)

#ESCENARIO ALTO: ihme alto con la misma proporcion por edad: muertes + muertes_sup
  #crear vectores vacios
  exihs2f<-vector("list",length(estados))
  exihs2m<-vector("list",length(estados))

  #hacer loop por estado para cada sexo
  for (i in estados) {
    tf_i<-filter(tf,entidad==i)
    lt5f_i<-LT(tf_i$poblacion,tf_i$muertes+tf_i$muertes_sup*(tf_i$covid/tf_i$tot), ages=c(0,1,seq(5,100,by=5)),sex = "female", axmethod = "preston", mxsmooth = FALSE, axsmooth = TRUE)
    exihs2f[[i]]<-lt5f_i$ex
  }
  for (i in estados) {
    tm_i<-filter(tm,entidad==i)
    lt5m_i<-LT(tm_i$poblacion,tm_i$muertes+tm_i$muertes_sup*(tm_i$covid/tm_i$tot), ages=c(0,1,seq(5,100,by=5)),sex = "male", axmethod = "preston", mxsmooth = FALSE, axsmooth = TRUE)
    exihs2m[[i]]<-lt5m_i$ex
  }
  #agregar numero de entidad a cada vector
  names(exihs2f) <-estados
  names(exihs2m) <-estados
  #convertir listas a dataframe
  #mujeres
  exf<-as.data.frame(exihs2f)
  exf<-exf[1,] #seleccionar e0
  exf<- gather(exf,X)
  exf<-exf%>%select(.,-c(X))
  names(exf)<-("e0_f_ihme_sup")
  #hombres
  exm<-as.data.frame(exihs2m)
  exm<-exm[1,]
  exm<- gather(exm,X)
  exm<-exm%>%select(.,-c(X))
  names(exm)<-("e0_m_ihme_sup")
  #hacer dataframe final
  e0_ihme_sup<-cbind(exf,exm)

####COMBINAR DATAFRAMES
  e0_mexico<-cbind(e0_proyecciones,e0_covid,e0_ihme,e0_ihme_sup ) #hay q actualizar
  e0_mexico$entidad<-estados-1
  e0_mexico <-round(e0_mexico[1:11],2)
  e0_mexico<-merge(e0_mexico,nombres)
  #crear las diferencias
  e0_mexico$dif_f_co<-round(e0_mexico$e0_f_covid-e0_mexico$e0_f_proyecciones,2)
  e0_mexico$dif_m_co<-round(e0_mexico$e0_m_covid-e0_mexico$e0_m_proyecciones,2)
  e0_mexico$dif_f_ih<-round(e0_mexico$e0_f_ihme-e0_mexico$e0_f_proyecciones,2)
  e0_mexico$dif_m_ih<-round(e0_mexico$e0_m_ihme-e0_mexico$e0_m_proyecciones,2)
  e0_mexico$dif_f_ihs<-round(e0_mexico$e0_f_ihme_sup-e0_mexico$e0_f_proyecciones,2)
  e0_mexico$dif_m_ihs<-round(e0_mexico$e0_m_ihme_sup-e0_mexico$e0_m_proyecciones,2)
  
  diferencias<-e0_mexico%>%select(.,c(e0_f_proyecciones,e0_m_proyecciones,dif_f_co,dif_m_co,dif_f_ih,dif_m_ih,dif_f_ihs,dif_m_ihs,nombres))
  #write_xlsx(diferencias,"covid_mexico/diferencias_e0.xlsx")


