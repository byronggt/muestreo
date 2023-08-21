# Script general sobre muestreo
# Dr. Sc. Ezequiel López
# Dr. Byron González
# http://cete.fausac.gt

# Cálculo del tamaño de muestra para estimar una media poblacional

mpiloto<-c(4120,3050,1250,3500,3100,1950,3175,2275,1275,1470)
length(mpiloto)
mediap<-mean(mpiloto)
var(mpiloto)
de<-sd(mpiloto)
e = 0.05
d<-round(mediap*e,2); d    # epsilon
nc<-0.95
ns<-1-nc
NP<-3000

tam.muestra=function(alfa,epsilon,s,N=Inf)
{
  za2=qnorm(1-alfa/2)
  if (N==Inf) n=(s*za2/epsilon)^2
  else n=N*((za2*s)^2)/((N-1)*epsilon^2+(za2*s)^2)
  return(ceiling(n))
}

# Muestreo de población infinita

n=tam.muestra(alfa=ns,epsilon=d,s=de)
n

# Muestreo población finita

n=tam.muestra(alfa=ns,epsilon=d,s=de,N=NP)
n

#-------------------------------------------------------------------------------

# Cálculo del tamaño de la muestra para un total

NPt<-5000
mpiloto1<-c(2.10,5.10,4.20,3.75,2.80,4.75,3.30,2.60,1.90,5.40)
length(mpiloto1)
mediap1<-mean(mpiloto1)
var(mpiloto1)
det<-sd(mpiloto1)
et = 0.05
dt<-round(NPt*mediap1*et,2); dt    

# Valor de z de la distribución normal

nct<-0.95               #nivel de confianza
nst<-1-nct              #nivel de significancia (alfa)
zt<-qnorm(1-nst/2);zt   #valor de z de la distribución normal estándar

nt=(NPt*zt*det)^2/(dt^2+NPt*(zt*det)^2)
ceiling(nt)

#-------------------------------------------------------------------------------

# Cálculo del tamaño de la muestra para una proporción binomial

mpiloto2<-c("NO","NO","NO","SI","NO","SI","NO","NO","SI","NO")
length(mpiloto2)

mpiloto2f<-as.factor(mpiloto2)
summary(mpiloto2f=="SI")

po<-3/10

# Cálculo de la muestra definitiva

# Valor de z de la distribución normal

Npo<-4200               #tamaño de la población objetivo
ncp<-0.95               #nivel de confianza
nsp<-1-ncp              #nivel de significancia
zp<-qnorm(1-nsp/2);zp   #valor de z de la distribución normal estándar

epo<-0.05               #error absoluto de muestreo

# Poblaciones infinitas

npo_inf<-(zp^2*po*(1-po))/epo^2
ceiling(npo_inf)

# Poblaciones finitas

npo_fin<-(Npo*zp^2*po*(1-po))/((Npo*epo^2)+(zp^2*po*(1-po)))
ceiling(npo_fin)

#-------------------------------------------------------------------------------

# Muestreo empleado en vegetación

tp<-0.1                 #tamaño de la parcela en hectáreas (0.1 ha =1,000 m2)
Nveg<-35/tp;Nveg

mpiloto3<-c(10,15,6,16,3,12,11,5,8,14)    #m3/parcela de 0.1 ha
mpiloto4<-mpiloto3*10                     #m3/hectárea

EE<-sd(mpiloto4)/sqrt(length(mpiloto4))   #error estándar de la media
CV<-(sd(mpiloto4)/mean(mpiloto4))*100     #coeficiente de variación en % 
gl1<-length(mpiloto4)-1                   #grados de libertad de la muestra inicial     

nct<-0.95                         #nivel de confianza
nst<-1-nct                        #nivel de significancia
t<-qt(1-nst/2,gl1);t              #valor de t de la distribución t de Student

Em<-t*EE                          #error de muestreo
Emp<- Em/mean(mpiloto4)*100;Emp   #error de muestreo relativo (respecto a la media y en %)

# n definitiva para poblaciones infinitas

Er_p<-15                          # error relativo de muestreo (máximo permitido en porcentaje)

nveg1<- (t*CV/Er_p)^2
ceiling(nveg1)                    # cantidad de muestras

# n definitiva para poblaciones finitas

nveg2<- (t*CV)^2/(Er_p^2+((t*CV)^2/Nveg))

nveg2_def<-ceiling(nveg2);nveg2_def     # cantidad de muestras definitiva 1

# Cálculo definitivo

gl1<-length(mpiloto4)-1        # grados de libertad de la muestra piloto"
gl2<-nveg2_def-1               # grados de libertad de la muestra definitiva 1
gl2
gl1

# promedio de los grados de libertad
gl3<-(gl1+gl2)/2
gl3

# Valor de t cuando se emplea el promedio del número de grados de libertad

t2<-qt(1-nst/2,gl3);t2

# Cálculo de la muestra final

nveg3<-(t2*CV)^2/(Er_p^2+((t2*CV)^2/Nveg))
ceiling(nveg3)    # Respuesta: 34 parcelas de 0.1 hectáreas corresponde muestrear

#-------------------------------------------------------------------------------