
#_____________________ MEDIDAS ___________________

# Se trabajará con la matriz de datos "penguins.xlsx"

#1.- Exportacion de matriz

# Import dataset/from excel/ Browser/ seleccionar
archivo/aceptar/ (visualizar)/ import

#2.- Acortar el nombre de la matriz de datos
BD<-penguins

#-----------------------------------------------
# Exploracion de la matriz
#-----------------------------------------------
dim(BD)
str(BD)
colnames(BD)
anyNA(BD)

#-----------------------------------------------
#      Tendencia central
#-----------------------------------------------

# 1.- Media y mediana
summary(BD)


# 2.- Moda

# 2.1.- Se descarga el paquete "modeest"
install.packages("modeest")

# 2.2.- Se abre la librería
library(modeest)

# 2.3.- Cálculo de la moda para la variable isla y largo del pico
mfv(BD$isla) # categorica
mfv(BD$largo_pico_mm) # numerica

mfv(BD$especie) # categorica
mfv(BD$masa_corporal_g) # numerica

#-----------------------------------------------
#      Medidas de dispersión
#-----------------------------------------------

# 1.- Cálculo de la varianza (sólo para variables cuantitativas)
var(BD$grosor_pico_mm)

# 2.- Cálculo de la desviación estándar
sd(BD$grosor_pico_mm)

# 3.- Error
media_pico<-mean(BD$largo_pico_mm)
media_pico

error<-(BD$largo_pico_mm-(media_pico))
error


#4.- Coeficiente de variacion
CV<- sd(BD$largo_pico_mm)/mean(BD$largo_pico_mm)*100
CV

# 5.- Rango intercuartilico (IQR)
IQR(BD$largo_pico_mm)

# 6.- Rango
pico<-BD$largo_pico_mm
pico
max(pico)
min(pico)

rango<-max(pico)-min(pico)
rango

#-----------------------------------------------
#    Medidas de posición
#------------------------------------------------

# 1.- Cuartiles
summary(BD)

# 2.- Quintil
quintil<-quantile(BD[["largo_aleta_mm"]], 
                  p=c(.20, .40, .60, .80))
quintil

# 3.- Decil
decil<-quantile(BD[["largo_aleta_mm"]], 
                p=c(.10, .20, .30, .40, .50, .60,
                    .70, .80, .90))
decil

# Percentil
percentil<-quantile(BD[["largo_aleta_mm"]], 
                    p=c(.33, .66, .99))
percentil

# Interpretacion:
# <192 = Bajo
# 192-209 = Intermedio
# > 209 = Alto

#---------------------------------------------------
# Ejercicio 1
#---------------------------------------------------

# 2.3.- Cálculo de la moda para la variable especie y masa corporal

mfv(BD$especie) # categorica
mfv(BD$masa_corporal_g) # numerica

# La moda de la variable especie es "Adelie"
# La moda de la variable masa corporal es 3700 3800

#-----------------------------------------------
#      Medidas de dispersión
#-----------------------------------------------

# 1.- Cálculo de la varianza (sólo para variables cuantitativas)
var(BD$masa_corporal_g)
# La varianza de la variable masa corporal es 641436.2

# 2.- Cálculo de la desviación estándar
sd(BD$masa_corporal_g)
# La desviacion estandar de la variable masa corporal es 800.8971

# 3.- Error
media_masa<-mean(BD$masa_corporal_g)
media_masa
# El error de la variable masa corproal es 4202.253

error<-(BD$masa_corporal_g-(media_masa))
error


#4.- Coeficiente de variacion
CV<- sd(BD$masa_corporal_g)/mean(BD$masa_corporal_g)*100
CV
# El coeficiente de variacion de la variable masa coporal es 19.05876

# 5.- Rango intercuartilico (IQR)
IQR(BD$masa_corporal_g)
# El rango intercuartilico (IQR) de la variable masa corporal es 1206.25

# 6.- Rango
masa<-BD$masa_corporal_g
masa
max(masa)
# El valor maximo de la variable masa corporal es 6300

min(masa)
# El valor minimo de la variable masa corporal es 2700

rango<-max(masa)-min(masa)
rango
# El valor del rango de la variable masa corporal es 3600


#-----------------------------------------------
#    Medidas de posición
#------------------------------------------------

# 1.- Cuartiles
summary(BD)
# los valores de cuartil para la varible masa corporal son
#  1st Qu.:3550
#  Median :4050
#  3rd Qu.:4756

# 2.- Quintil
quintil<-quantile(BD[["masa_corporal_g"]], 
                  p=c(.20, .40, .60, .80))
quintil
# Los valores de quintil para la variable masa corporal son 
#  20%  40%  60%  80% 
# 3475 3800 4300 4950


# 3.- Decil
decil<-quantile(BD[["masa_corporal_g"]], 
                p=c(.10, .20, .30, .40, .50, .60,
                    .70, .80, .90))
decil
# Los valores de decil para la variable masa corporal son
#  10%  20%  30%  40%  50%  60%  70%  80%  90%
# 3300 3475 3650 3800 4050 4300 4650 4950 5400

# Percentil
percentil<-quantile(BD[["masa_corporal_g"]], 
                    p=c(.33, .66, .99))
percentil

# Los valores de percentil para la variable masa corporal son
#  33%      66%       99%
#  3700.0  4500.0    5978.5 

# Interpretacion:
# <3700.0 = Bajo
# 3700.0-5978.5 = Intermedio
# > 5978.5 = Alto