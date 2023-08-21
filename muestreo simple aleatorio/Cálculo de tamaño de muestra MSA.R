# Dr. Ezequiel López
# Dr. Byron González
# http://cete.fausac.gt

# Script para el cálculo del tamaño de la muestra MSA

rm(list=ls(all=TRUE)); graphics.off(); shell("cls")

# Bibliotecas necesarias:

if(!require(TeachingSampling)){install.packages("TeachingSampling")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(DescTools)){install.packages("DescTools")}         #biblioteca para el cálculo de los intervalos de confianza
if(!require(samplingbook)){install.packages("samplingbook")}   #biblioteca para el cálculo del tamaño de la muestra

if(!require(devtools)){install.packages("devtools")}   
install_github("DFJL/SamplingUtil")
if(!require(SamplingUtil)){install.packages("SamplingUtil")}   #otra biblioteca para calcular el tamaño de la muestra

# Material de consulta:
# https://rstudio-pubs-static.s3.amazonaws.com/916099_e92926d93115437583d8fbf4db8d8e46.html

# Importar los datos para realizar los ejercicios

data(Lucy)      # Invocar los datos Lucy  
data(BigLucy)   # Invocar los datos BigLucy
head(Lucy)      # Ver las primeras 5 filas

glimpse(Lucy)   # revisar la estructura de cada variable

# Se observa un conjunto de datos con 2396 filas de compañías industriales con diferentes 
# variables financieras de una ciudad en una año fiscal. (note que Lucy es una muestra 
# aleatoria simple del conjunto datos BigLucy)

set.seed(123) # Definir el valor de la semilla para obtener resultados reproducibles.

# Con el comando slice_sample() de la biblioteca dplyr se selecciona una muestra 
# de todas las variables del data frame

Lucy  %>% 
  slice_sample(n=10)-> muestra
muestra

# sample() ejecuta solamente el muestreo de la variable de interés (income = ingreso) 
# Es importante observar que los resultados son los mismos al definir la semilla o valor inicial

set.seed(123)
sample(Lucy$Income,size=10) 

# Estimaciones con una muestra aleatoria simple

MeanCI(muestra$Income,conf.level = 0.95)  #  Vector e intervalo de 95% de confianza

# Otra forma de calcular los IC
t.test(muestra$Income,conf.level = 0.95)  #  Vector e intervalo de confianza

# Cálculo del tamaño de la muestra (MSA) para la estimación de la media

N=nrow(Lucy)                                     # tamaño de la población
s=sd(Lucy$Income)*sqrt(N-1)/sqrt(N)              # sigma de la población
e=20                                             # error absoluto (en unidades)
sample.size.mean(e=e, S=s, N = N, level = 0.95)  # cálculo del tamaño de la muestra

# Usar la biblioteca SamplingUtil (abs = error absoluto en unidades, alpha es el nivel de significancia)

nsize(Lucy$Income, abs=20, alpha = 0.05)

# Conocido el tamaño de muestra, se repite la estimación de la media, con una muestra de ese tamaño

set.seed(123)
muestra1<-sample(Lucy$Income, size = 533)  # muestreo  con el comando Sample
MeanCI(muestra1,       conf.level = 0.95)  # nivel de confianza

error_e= (450.6679-402.2101)/2  # error estándar
error_e

#-------------------------------------------------------------------------------------------------------------------------

# Ejemplo de Cálculo de muestra y estimación para una proporción

# En el conjunto de datos Lucy se distingue la variable SPAM (propaganda con internet) que es del tipo factor con dos categorías. 
# Se desea estimar la proporción de la población de empresas que utiliza el Internet para hacer propaganda.

# Se inicia con el cálculo del tamaño de la muestra (Es necesario definir el valor del error relativo) , fijado en 2%.
# Luego, se realiza el muestreo, al considerar una proporción de éxito p = 0.5
# Calcular el intervalo de confianza.

n<-sample.size.prop(e=.02,P = 0.5,N = N,level = 0.95)$n  # tamaño de la muestra
n

set.seed(1234)
muestra2<-sample(Lucy$SPAM,size =n)
prop.test(table(muestra2),conf.level = 0.95)

#-------------------------------------------------------------------------------
