  ############Tema: Despliegue de Modelos Predictivos

library(dplyr)
library(ggplot2)
library(ggpubr)
library(rgl)
library(mvoutlier)
library(DMwR)
library(cluster)
library(foreign)
library(haven)
library(sjmisc)
library(dplyr)
library(rgl)
library(reshape)
library(VIM)
library(DEoptimR)
library(minqa)
library(nloptr)
library(gdata)
library(gridExtra)
library(TTR)
library(caTools)
library(arules)
library(readr)
library(agricolae)
library(fBasics)
library(ROSE)

  #Soluciones basadas en modelo
  
  #01 File/Save with Encoding..../UTF-8
  #C:/Users/ORLANDO ADVINCULA/R SODATA/3. avanzado/despliegue del modelo
  getwd()

  #02 Rutear a la carpeta de trabajoaaaaaa
  
  #03 LECTURA
  dfm<-read.csv("para_modelo190x.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE)
  head(dfm)
  # Ejemplo 1:
  
  dfm<-dfm %>%
    #select() %>%
    mutate(
      TARGET = case_when(
        dfm$DESC_Causa=='TRASPASO EGRESO' ~1,
        TRUE ~  0 ),
      TARGETCLASS = case_when(
        dfm$DESC_Causa=='TRASPASO EGRESO' ~"fuga",
        TRUE ~  "nofuga" ),
      SUBFINAL=case_when(dfm$subcategoria=='P0' | dfm$subcategoria=='P2'| dfm$subcategoria=='P1'| dfm$subcategoria=='P3' ~"P",
                         TRUE ~  "A" )
    )
  
  dfm$TARGET<-as.factor(dfm$TARGET)
  dfm$FONDO<-as.factor(dfm$FONDO)
  dfm$SUBFINAL<-as.factor(dfm$SUBFINAL)
  dfm$TARGETCLASS<-as.factor(dfm$TARGETCLASS)
  
  
  dfm1<-dfm[,-c(1,2,3,9,10,16,17,18,20,22,24,25,28)]
  
  str(dfm1)
  names(dfm1)
  
  dfm3<-dfm1[,c(3,4,5,6,8,10,11,12,13,14,15)]
  str(dfm3)
  names(dfm3)
  #Estimar el ÃÂÃÂÃÂÃÂÃÂÃÂÃÂÃÂ¡rbol
  # V7: 1 (higado enfermo) 2 (higado sano): Variable Target
  # V3: alamine aminotransferase
  # V5: gamma-glutamyl transpeptidase
  
  
  #05 Arbol de decisiones
  library(rpart)
  library(partykit)
  reclamo4 <- glm(TARGET~. , family = binomial,data=dfm3)
  summary(reclamo4)
  
  
  # Guardar el modelo para su despliegue
  #      #MODELO          #NOMBRE DEL MODELO
  save(reclamo4, file='regresion_logistica_FC.Rdata')
  #Formato de los modelos R.Data
