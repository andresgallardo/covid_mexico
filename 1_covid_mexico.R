### Script para analizar COVID en Mexico
# library(tidyr)
# library(data.table)
# library(tidyquant)
# library(dplyr)
# library(earlyR)
# library(incidence)
# library(EpiEstim)
# library(collapse)
# library(writexl)

#setwd("~/R folder")
#PREPARAR DATAFRAME DE CASOS DE COVID EN MEXICO
comex<-read.csv(file = "covid_mexico/200720COVID19MEXICO.csv") #ultimo disponible
  # casos positivos
  comex_todos<-comex
  comex<-comex%>%filter(RESULTADO==1)
  # muertes
  comex_mu <- subset(comex, FECHA_DEF!="9999-99-99")
  #comex_mu<-comex_mu%>%filter(FECHA_DEF<"2020/6/30") 
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
proyec<-read.csv("covid_mexico/proyecciones.csv")
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
  covid_mort_g<-collap(covid_mort,DEFUNCIONES+POBLACION+MUERTES~YEAR+ENTIDAD+CVE_GEO+SEXO+EDAD_G,sum)
  
  #mortalidad covid: mortalidad MINIMA para 2020
  covid_mort_g$MORTALIDAD_PROY<-covid_mort_g$DEFUNCIONES/covid_mort_g$POBLACION
    covid_mort_g$DEF_COV<-covid_mort_g$DEFUNCIONES+covid_mort_g$MUERTES
  covid_mort_g$MORTALIDAD_COV<-(covid_mort_g$DEF_COV)/covid_mort_g$POBLACION
  
  #guardar para revisar
      #crear edad para revisar en excel
      covid_mort_g$E<-substr(covid_mort_g$EDAD_G,1,2)
      covid_mort_g$E[covid_mort_g$EDAD_G == "100 +"] <- "100"
      covid_mort_g$E<-as.integer(covid_mort_g$E)
  write_xlsx(covid_mort_g,"covid_mexico/covid_mort_g.xlsx")

