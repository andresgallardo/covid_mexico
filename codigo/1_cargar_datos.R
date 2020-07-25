## SCRIPT PARA CARGAR DATOS DE CONAPO Y SECRETARIA DE SALUD
## NOTAS: datos de poblacion y mortalidad de proyecciones de conapo y 
##        de numero de muertes de secretaria de salud al 22 de julio
## autor: andrés gallardo

# library(tidyr)
# library(data.table)
# library(tidyquant)
# library(dplyr)
# library(collapse)
# library(writexl)

#setwd("~/R folder") #definir working directory

#PREPARAR DATAFRAME DE CASOS DE COVID EN MEXICO
comex<-read.csv(file = "covid_mexico/datos/200722COVID19MEXICO.csv") # 22 julio. mas reciente: https://www.gob.mx/salud/documentos/datos-abiertos-152127
  #casos positivos
  comex_todos<-comex
  comex<-comex%>%filter(RESULTADO==1)
  #muertes
  comex_mu <- subset(comex, FECHA_DEF!="9999-99-99")
  comex_mu$FECHA_DEF<-as.Date(comex_mu$FECHA_DEF)
  comex_mu$EDAD[comex_mu$EDAD >= 100] <- 100
  comex_mu$MUERTES=1
  #collapse para estados
  comex_edos<-collap(comex_mu,MUERTES~EDAD+ENTIDAD_RES+SEXO,sum)
  comex_edos$YEAR=2020
  comex_edos$SEXO<-as.character(comex_edos$SEXO)
  #collapse para nacional
  comex_nal<-collap(comex_edos,MUERTES~EDAD+SEXO+YEAR,sum)
  comex_nal$ENTIDAD_RES=0
  comex_nal <- comex_nal[c(1,5,2,3,4)]
    comex_edos<-rbind(comex_edos,comex_nal)
  
#PREPARAR DATAFRAME DE PROYECCIONES DE POBLACION Y MUERTES 
proyec<-read.csv("covid_mexico/proyecciones.csv") #trabajo previo en este archivo para juntar los datos de Defunciones 1950-2050 y Población a mitad de año de CONAPO
  #limpiar proyec para 2000-2020
  proyec<-proyec%>%filter(YEAR>=2000&YEAR<=2020)
  #limpiar edades
  proyec$EDAD[proyec$EDAD >= 100] <- 100
    #collapse para juntar todos los arriba de 100
    proyec<-collap(proyec,DEFUNCIONES+POBLACION~YEAR+ENTIDAD+CVE_GEO+SEXO+EDAD,sum)
  
  #preparar para el merge
  proyec$ENTIDAD_RES<-proyec$CVE_GEO
  proyec$SEXO[proyec$SEXO=="Mujeres"]<-"1"
  proyec$SEXO[proyec$SEXO=="Hombres"]<-"2"
  proyec_20<-proyec%>%filter(YEAR==2020)
  
#MERGE PARA ESTIMADO MORTALIDAD
  covid_mort<-merge(proyec_20,comex_edos, all=TRUE)
    covid_mort[is.na(covid_mort)] <- 0
  
    #grupos de edad
    agebreaks <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,199)
    agelabels <- c("00","01-04","05-09","10-14","15-19","20-24","25-29","30-34",
                   "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                   "70-74","75-79","80-84","85-89","90-95","95-99","100 +")
    
    setDT(covid_mort)[ , EDAD_G := cut(EDAD, 
                                       breaks = agebreaks, 
                                       right = FALSE, 
                                       labels = agelabels)]
  #hacer collapse para tener totales
  covid_mort<-collap(covid_mort,DEFUNCIONES+POBLACION+MUERTES~YEAR+ENTIDAD+CVE_GEO+SEXO+EDAD_G,sum)
  
  # #mortalidad covid: mortalidad MINIMA para 2020
  # covid_mort$MORTALIDAD_PROY<-covid_mort$DEFUNCIONES/covid_mort$POBLACION
  #   covid_mort$DEF_COV<-covid_mort$DEFUNCIONES+covid_mort$MUERTES
  # covid_mort$MORTALIDAD_COV<-(covid_mort$DEF_COV)/covid_mort$POBLACION
  # 
  # #guardar para revisar
  #     #crear edad para revisar en excel
  #     covid_mort$E<-substr(covid_mort$EDAD_G,1,2)
  #     covid_mort$E[covid_mort$EDAD_G == "100 +"] <- "100"
  #     covid_mort$E<-as.integer(covid_mort$E)
  # #write_xlsx(covid_mort,"covid_mexico/covid_mort.xlsx")

#CARGAR DATAFRAME covid_mort DEL SCRIPT 1_cargar_datos
t<-covid_mort%>%select(.,c(SEXO, CVE_GEO,DEFUNCIONES,POBLACION,MUERTES,DEF_COV,E))
  names(t)<-c("sexo", "entidad","muertes","poblacion", "covid", "muertes_totales", "edad")
  t<-arrange(t,entidad)
  t$entidad_o<-t$entidad #crear variable para que no rompa con entidad 0
  t$entidad<-t$entidad+1
  
#CARGAR DATOS DEL IHME: https://covid19.healthdata.org/mexico
ihm<-read.csv("covid_mexico/ihme_jul22.csv") #Reference_hospitalization_all_locs.csv filtrado para mexico y sus entidades federativas. agrego variable CVE_GEO para referencia
  ihme<-ihm%>%select(.,c(CVE_GEO,totdea_mean,totdea_lower,totdea_upper))
  names(ihme)<-c("entidad_o","muertes_nov", "muertes_inf", "muertes_sup")
  names(ihm)<-c("nombres","entidad")
  nombres<-select(ihm,c(nombres,entidad))
  ihme <-round(ihme[1:4],0)
  ihme$edad=0
  ihme$entidad=ihme$entidad+1
  
#AGREGAR PROPORCIONES A t PORQUE IHME NO DESCOMPONE POR EDAD/SEXO
t<-t%>%group_by(entidad)%>%mutate(tot=sum(covid))
  t<-merge(ihme,t,all=TRUE)
  cols <- c("muertes_nov","muertes_sup", "muertes_inf")
  t<- t%>% group_by(entidad) %>% mutate_at(cols, na.aggregate)
  
