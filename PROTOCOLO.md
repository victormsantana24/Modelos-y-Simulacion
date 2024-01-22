## Instalamos las paqueterias principales para el desarrollo del trabajo
install.packages("MVA")
library("MVA")
install.packages("mvtnorm")
install.packages("readr")
library(readr)

## Seleccionamos la base de datos previamente guardada
file.choose()

## Cambiamos la ruta al abrir en otro dispositivo
rutabase = "/Users/victormanuel/Desktop/PROTOCOLO/Base de datos/Copia de Poblacion.csv"
base = read.csv(rutabase)

## Visualizamos la base de datos
View(base)

##Eliminamos la fila 1 y nombramos una nueva base
base2<- base[-c(1),]
base2
View(base2)

## EDO. AGUASCALIENTES

a=7^5
m=2^31-1
S0AG=200003173   # Simulacion i
n=10000
# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0AG=rep(0,n+1)
vec_S0AG[1]=S0AG
for(i in 2:(n+1)){
  vec_S0AG[i]=as.double(a*vec_S0AG[i-1])%%m
}
vec_S0AG=vec_S0AG/m
vec_S0AG<-vec_S0AG[-1]
plot(vec_S0AG)
hist(vec_S0AG)

## SELECCIONAR i:

vec_i=rep(0,length(vec_S0AG))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0AG[i]<.5) {
    vec_i[i]=base2[2,13]
  } else {
    vec_i[i]=base2[2,14]
    
  }}

## ESTIMAR POBLACION 2030:

Poblacion2020AG=base2[2,4]
Poblacion2020AG

Poblacion2030AG=((1+vec_i)^10)*(Poblacion2020AG)
Poblacion2030AG

hist(Poblacion2030AG,main= "Población 2030 Aguas Calientes", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
x11()
## EDO BAJA CALIFORNIA

a=7^5
m=2^31-1
S0BC=200003173+2*2   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0BC=rep(0,n+1)
vec_S0BC[1]=S0BC
for(i in 2:(n+1)){
  vec_S0BC[i]=as.double(a*vec_S0BC[i-1])%%m
}
vec_S0BC=vec_S0BC/m
vec_S0BC<-vec_S0BC[-1]
plot(vec_S0BC)
hist(vec_S0BC)

## SELECCIONAR i

vec_iBC=rep(0,length(vec_S0BC))

for(i in 1:n){
  
  if(vec_S0BC[i]<.5) {
    vec_iBC[i]=base2[3,13]
  } else {
    vec_iBC[i]=base2[3,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020BC=base2[3,4]
Poblacion2020BC

Poblacion2030BC=((1+vec_i)^10)*(Poblacion2020BC)
Poblacion2030BC

hist(Poblacion2030BC)
hist(Poblacion2030BC,main= "Población 2030 Baja California", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030BC)

# EDO. BAJA CALIFORNIA SUR

a=7^5
m=2^31-1
S0BCS=200003173+2*3   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0BCS=rep(0,n+1)
vec_S0BCS[1]=S0BCS
for(i in 2:(n+1)){
  vec_S0BCS[i]=as.double(a*vec_S0BCS[i-1])%%m
}
vec_S0BCS=vec_S0BCS/m
vec_S0BCS<-vec_S0BCS[-1]
plot(vec_S0BCS)
hist(vec_S0BCS)

## SELECCIONAR i

vec_iBCS=rep(0,length(vec_S0BCS))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0BCS[i]<.5) {
    vec_iBCS[i]=base2[4,13]
  } else {
    vec_iBCS[i]=base2[4,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020BCS=base2[4,4]
Poblacion2020BCS

Poblacion2030BCS=((1+vec_iBCS)^10)*(Poblacion2020BCS)
Poblacion2030BCS

hist(Poblacion2030BCS)
hist(Poblacion2030BCS,main= "Población 2030 Baja California Sur", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030BCS)

# EDO. CAMPECHE

a=7^5
m=2^31-1
S0CA=200003173+2*4   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0CA=rep(0,n+1)
vec_S0CA[1]=S0CA
for(i in 2:(n+1)){
  vec_S0CA[i]=as.double(a*vec_S0CA[i-1])%%m
}
vec_S0CA=vec_S0CA/m
vec_S0CA<-vec_S0CA[-1]
plot(vec_S0CA)
hist(vec_S0CA)

## SELECCIONAR i

vec_iCA=rep(0,length(vec_S0CA))

for(i in 1:n){
  
  if(vec_S0CA[i]<.5) {
    vec_iCA[i]=base2[5,13]
  } else {
    vec_iCA[i]=base2[5,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020CA=base2[5,4]
Poblacion2020CA

Poblacion2030CA=((1+vec_iCA)^10)*(Poblacion2020CA)
Poblacion2030CA

hist(Poblacion2030CA)
hist(Poblacion2030CA,main= "Población 2030 Campeche", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030CA)

# EDO. COAHUILA

a=7^5
m=2^31-1
S0CO=200003173+2*5   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0CO=rep(0,n+1)
vec_S0CO[1]=S0CO
for(i in 2:(n+1)){
  vec_S0CO[i]=as.double(a*vec_S0CO[i-1])%%m
}
vec_S0CO=vec_S0CO/m
vec_S0CO<-vec_S0CO[-1]
plot(vec_S0CO)
hist(vec_S0CO)

## SELECCIONAR i

vec_iCO=rep(0,length(vec_S0CO))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0CO[i]<.5) {
    vec_iCO[i]=base2[6,13]
  } else {
    vec_iCO[i]=base2[6,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020CO=base2[6,4]

Poblacion2030CO=((1+vec_iCO)^10)*(Poblacion2020CO)
Poblacion2030CO

hist(Poblacion2030CO)
hist(Poblacion2030CO,main= "Población 2030 Coahuila", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030CO)

# EDO. COLIMA

a=7^5
m=2^31-1
S0CL=200003173+2*6   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0CL=rep(0,n+1)
vec_S0CL[1]=S0CL
for(i in 2:(n+1)){
  vec_S0CL[i]=as.double(a*vec_S0CL[i-1])%%m
}
vec_S0CL=vec_S0CL/m
vec_S0CL<-vec_S0CL[-1]
plot(vec_S0CL)
hist(vec_S0CL)

## SELECCIONAR i

vec_iCL=rep(0,length(vec_S0CL))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0CL[i]<.5) {
    vec_iCL[i]=base2[7,13]
  } else {
    vec_iCL[i]=base2[7,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020CL=base2[7,4]
Poblacion2020CL

Poblacion2030CL=((1+vec_iCL)^10)*(Poblacion2020CL)
Poblacion2030CL

hist(Poblacion2030CL)
hist(Poblacion2030CL,main= "Población 2030 Colima", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030CL)

# EDO. CHIAPAS

a=7^5
m=2^31-1
S0CS=200003173+2*7   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0CS=rep(0,n+1)
vec_S0CS[1]=S0CS
for(i in 2:(n+1)){
  vec_S0CS[i]=as.double(a*vec_S0CS[i-1])%%m
}
vec_S0CS=vec_S0CS/m
vec_S0CS<-vec_S0CS[-1]
plot(vec_S0CS)
hist(vec_S0CS)

## SELECCIONAR i

vec_iCS=rep(0,length(vec_S0CS))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0CS[i]<.5) {
    vec_iCS[i]=base2[8,13]
  } else {
    vec_iCS[i]=base2[8,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020CS=base2[8,4]
Poblacion2020CS

Poblacion2030CS=((1+vec_i)^10)*(Poblacion2020CS)
Poblacion2030CS

hist(Poblacion2030CS)
hist(Poblacion2030CS,main= "Población 2030 Chiapas", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030CS)

# EDO. CHIHUAHUA

a=7^5
m=2^31-1
S0CH=200003173+2*8  # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0CH=rep(0,n+1)
vec_S0CH[1]=S0CH
for(i in 2:(n+1)){
  vec_S0CH[i]=as.double(a*vec_S0CH[i-1])%%m
}
vec_S0CH=vec_S0CH/m
vec_S0CH<-vec_S0CH[-1]
plot(vec_S0CH)
hist(vec_S0CH)

## SELECCIONAR i

vec_iCH=rep(0,length(vec_S0CH))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0CH[i]<.5) {
    vec_iCH[i]=base2[9,13]
  } else {
    vec_iCH[i]=base2[9,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020CH=base2[9,4]
Poblacion2020CH

Poblacion2030CH=((1+vec_iCH)^10)*(Poblacion2020CH)
Poblacion2030CH

hist(Poblacion2030CH)
hist(Poblacion2030CH,main= "Población 2030 Chihuahua", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030CH)

# CIUDAD DE MEXICO

a=7^5
m=2^31-1
S0CM=200003173+2*9  # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0CM=rep(0,n+1)
vec_S0CM[1]=S0CM
for(i in 2:(n+1)){
  vec_S0CM[i]=as.double(a*vec_S0CM[i-1])%%m
}
vec_S0CM=vec_S0CM/m
vec_S0CM<-vec_S0CM[-1]
plot(vec_S0CM)
hist(vec_S0CM)

## SELECCIONAR i

vec_iCM=rep(0,length(vec_S0CM))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0CM[i]<.5) {
    vec_iCM[i]=base2[10,13]
  } else {
    vec_iCM[i]=base2[10,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020CM=base2[10,4]
Poblacion2020CM

Poblacion2030CM=((1+vec_iCM)^10)*(Poblacion2020CM)
Poblacion2030CM

hist(Poblacion2030CM)
hist(Poblacion2030CM,main= "Población 2030 Ciudad de México", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030CM)

# EDO. DURANGO

a=7^5
m=2^31-1
S0DG=200003173+2*10   # Simulacion i
n=10000


# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0DG=rep(0,n+1)
vec_S0DG[1]=S0DG
for(i in 2:(n+1)){
  vec_S0DG[i]=as.double(a*vec_S0DG[i-1])%%m
}
vec_S0DG=vec_S0DG/m
vec_S0DG<-vec_S0DG[-1]
plot(vec_S0DG)
hist(vec_S0DG)

## SELECCIONAR i

vec_iDG=rep(0,length(vec_S0DG))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0DG[i]<.5) {
    vec_iDG[i]=base2[11,13]
  } else {
    vec_iDG[i]=base2[11,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020DG=base2[11,4]
Poblacion2020DG

Poblacion2030DG=((1+vec_iDG)^10)*(Poblacion2020DG)
Poblacion2030DG

hist(Poblacion2030DG)
hist(Poblacion2030DG,main= "Población 2030 Durango", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030DG)


# EDO. GUANAJUATO

a=7^5
m=2^31-1
S0GTO=200003173+2*11   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0GTO=rep(0,n+1)
vec_S0GTO[1]=S0GTO
for(i in 2:(n+1)){
  vec_S0GTO[i]=as.double(a*vec_S0GTO[i-1])%%m
}
vec_S0GTO=vec_S0GTO/m
vec_S0GTO<-vec_S0GTO[-1]
plot(vec_S0GTO)
hist(vec_S0GTO)

## SELECCIONAR i

vec_iGTO=rep(0,length(vec_S0GTO))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0GTO[i]<.5) {
    vec_iGTO[i]=base2[12,13]
  } else {
    vec_iGTO[i]=base2[12,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020GTO=base2[12,4]
Poblacion2020GTO

Poblacion2030GTO=((1+vec_iGTO)^10)*(Poblacion2020GTO)
Poblacion2030GTO

hist(Poblacion2030GTO)
hist(Poblacion2030GTO,main= "Población 2030 Guanajuato", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030GTO)


# EDO. GUERRERO

a=7^5
m=2^31-1
S0GRO=200003173+2*12   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0GRO=rep(0,n+1)
vec_S0GRO[1]=S0GRO
for(i in 2:(n+1)){
  vec_S0GRO[i]=as.double(a*vec_S0GRO[i-1])%%m
}
vec_S0GRO=vec_S0GRO/m
vec_S0GRO<-vec_S0GRO[-1]
plot(vec_S0GRO)
hist(vec_S0GRO)

## SELECCIONAR i

vec_iGRO=rep(0,length(vec_S0GRO))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0GRO[i]<.5) {
    vec_iGRO[i]=base2[13,13]
  } else {
    vec_iGRO[i]=base2[13,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020GRO=base2[13,4]
Poblacion2020GRO

Poblacion2030GRO=((1+vec_iGRO)^10)*(Poblacion2020GRO)
Poblacion2030GRO

hist(Poblacion2030GRO)
hist(Poblacion2030GRO,main= "Población 2030 Guerrero", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030GRO)

# EDO. HIDALGO

a=7^5
m=2^31-1
S0HG=200003173+2*13   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0HG=rep(0,n+1)
vec_S0HG[1]=S0HG
for(i in 2:(n+1)){
  vec_S0HG[i]=as.double(a*vec_S0HG[i-1])%%m
}
vec_S0HG=vec_S0HG/m
vec_S0HG<-vec_S0HG[-1]
plot(vec_S0HG)
hist(vec_S0HG)

## SELECCIONAR i

vec_iHG=rep(0,length(vec_S0HG))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0HG[i]<.5) {
    vec_iHG[i]=base2[14,13]
  } else {
    vec_iHG[i]=base2[14,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020HG=base2[14,4]
Poblacion2020HG

Poblacion2030HG=((1+vec_iHG)^10)*(Poblacion2020HG)
Poblacion2030HG

hist(Poblacion2030HG)
hist(Poblacion2030HG,main= "Población 2030 Hidalgo", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030HG)

# EDO. JALISCO

a=7^5
m=2^31-1
S0JA=200003173+2*14   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0JA=rep(0,n+1)
vec_S0JA[1]=S0JA
for(i in 2:(n+1)){
  vec_S0JA[i]=as.double(a*vec_S0JA[i-1])%%m
}
vec_S0JA=vec_S0JA/m
vec_S0JA<-vec_S0JA[-1]
plot(vec_S0JA)
hist(vec_S0JA)

## SELECCIONAR i

vec_iJA=rep(0,length(vec_S0JA))

for(i in 1:n){
  
  if(vec_S0JA[i]<.5) {
    vec_iJA[i]=base2[15,13]
  } else {
    vec_iJA[i]=base2[15,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020JA=base2[15,4]
Poblacion2020JA

Poblacion2030JA=((1+vec_iJA)^10)*(Poblacion2020JA)
Poblacion2030JA

hist(Poblacion2030JA)
hist(Poblacion2030JA,main= "Población 2030 Jalisco", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030JA)

# ESTADO DE MEXICO

a=7^5
m=2^31-1
S0EM=200003173+2*15   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0EM=rep(0,n+1)
vec_S0EM[1]=S0EM
for(i in 2:(n+1)){
  vec_S0EM[i]=as.double(a*vec_S0EM[i-1])%%m
}
vec_S0EM=vec_S0EM/m
vec_S0EM<-vec_S0EM[-1]
plot(vec_S0EM)
hist(vec_S0EM)

## SELECCIONAR i

vec_iEM=rep(0,length(vec_S0EM))

for(i in 1:n){
  
  if(vec_S0EM[i]<.5) {
    vec_iEM[i]=base2[16,13]
  } else {
    vec_iEM[i]=base2[16,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020EM=base2[16,4]
Poblacion2020EM

Poblacion2030EM=((1+vec_iEM)^10)*(Poblacion2020EM)
Poblacion2030EM

hist(Poblacion2023EM)
hist(Poblacion2030EM,main= "Población 2030 Estado de México", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030EM)

# EDO. MICHOACAN

a=7^5
m=2^31-1
S0MI=200003173+2*16   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0MI=rep(0,n+1)
vec_S0MI[1]=S0MI
for(i in 2:(n+1)){
  vec_S0MI[i]=as.double(a*vec_S0MI[i-1])%%m
}
vec_S0MI=vec_S0MI/m
vec_S0MI<-vec_S0MI[-1]
plot(vec_S0MI)
hist(vec_S0MI)

## SELECCIONAR i

vec_iMI=rep(0,length(vec_S0MI))

for(i in 1:n){
  
  if(vec_S0MI[i]<.5) {
    vec_iMI[i]=base2[17,13]
  } else {
    vec_iMI[i]=base2[17,14]
    
  }}

## ESTIMAR POBLACION 2023

Poblacion2020MI=base2[17,4]
Poblacion2020MI

Poblacion2030MI=((1+vec_iMI)^10)*(Poblacion2020MI)
Poblacion2030MI

hist(Poblacion2030MI)
hist(Poblacion2030MI,main= "Población 2030 Michoacan", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030MI)

# EDO. MORELOS

a=7^5
m=2^31-1
S0MO=200003173+2*17   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0MO=rep(0,n+1)
vec_S0MO[1]=S0MO
for(i in 2:(n+1)){
  vec_S0MO[i]=as.double(a*vec_S0MO[i-1])%%m
}
vec_S0MO=vec_S0MO/m
vec_S0MO<-vec_S0MO[-1]
plot(vec_S0MO)
hist(vec_S0MO)

## SELECCIONAR i

vec_iMO=rep(0,length(vec_S0MO))

for(i in 1:n){
  
  if(vec_S0MO[i]<.5) {
    vec_iMO[i]=base2[18,13]
  } else {
    vec_iMO[i]=base2[18,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020MO=base2[18,4]
Poblacion2020MO

Poblacion2030MO=((1+vec_iMO)^10)*(Poblacion2020MO)
Poblacion2030MO

hist(Poblacion2030MO)
hist(Poblacion2030MO,main= "Población 2030 Morelos", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030MO)

## EDO. Nayarit

a=7^5
m=2^31-1
S0NA=200003173+2*18   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0NA=rep(0,n+1)
vec_S0NA[1]=S0NA
for(i in 2:(n+1)){
  vec_S0NA[i]=as.double(a*vec_S0NA[i-1])%%m
}
vec_S0NA=vec_S0NA/m
vec_S0NA<-vec_S0NA[-1]
plot(vec_S0NA)
hist(vec_S0NA)

## SELECCIONAR i

vec_iNA=rep(0,length(vec_S0NA))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0NA[i]<.5) {
    vec_iNA[i]=base2[19,13]
  } else {
    vec_iNA[i]=base2[19,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020NA=base2[19,4]
Poblacion2020NA

Poblacion2030NA=((1+vec_iNA)^10)*(Poblacion2020NA)
Poblacion2030NA

hist(Poblacion2030NA)
hist(Poblacion2030NA,main= "Población 2030 Nayarit", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030NA)

## EDO. Nuevo León

a=7^5
m=2^31-1
S0NL=200003173+2*19   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0NL=rep(0,n+1)
vec_S0NL[1]=S0NL
for(i in 2:(n+1)){
  vec_S0NL[i]=as.double(a*vec_S0NL[i-1])%%m
}
vec_S0NL=vec_S0NL/m
vec_S0NL<-vec_S0NL[-1]
plot(vec_S0NL)
hist(vec_S0NL)

## SELECCIONAR i

vec_iNL=rep(0,length(vec_S0NL))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0NL[i]<.5) {
    vec_iNL[i]=base2[20,13]
  } else {
    vec_iNL[i]=base2[20,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020NL=base2[20,4]
Poblacion2020NL

Poblacion2030NL=((1+vec_iNL)^10)*(Poblacion2020NL)
Poblacion2030NL

hist(Poblacion2030NL)
hist(Poblacion2030NL,main= "Población 2030 Nuevo León", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030NL)

# EDO. OAXACA

a=7^5
m=2^31-1
S0OA=200003173+2*20   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0OA=rep(0,n+1)
vec_S0OA[1]=S0OA
for(i in 2:(n+1)){
  vec_S0OA[i]=as.double(a*vec_S0OA[i-1])%%m
}
vec_S0OA=vec_S0OA/m
vec_S0OA<-vec_S0OA[-1]
plot(vec_S0OA)
hist(vec_S0OA)

## SELECCIONAR i

vec_iOA=rep(0,length(vec_S0OA))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0OA[i]<.5) {
    vec_iOA[i]=base2[21,13]
  } else {
    vec_iOA[i]=base2[21,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020OA=base2[21,4]
Poblacion2020OA

Poblacion2030OA=((1+vec_iOA)^10)*(Poblacion2020OA)
Poblacion2030OA

hist(Poblacion2030OA)
hist(Poblacion2030OA,main= "Población 2030 Oaxaca", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030OA)

# EDO. PUEBLA

a=7^5
m=2^31-1
S0PU=200003173+2*21   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0PU=rep(0,n+1)
vec_S0PU[1]=S0PU
for(i in 2:(n+1)){
  vec_S0PU[i]=as.double(a*vec_S0PU[i-1])%%m
}
vec_S0PU=vec_S0PU/m
vec_S0PU<-vec_S0PU[-1]
plot(vec_S0PU)
hist(vec_S0PU)

## SELECCIONAR i

vec_iPU=rep(0,length(vec_S0PU))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0PU[i]<.5) {
    vec_iPU[i]=base2[22,13]
  } else {
    vec_iPU[i]=base2[22,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020PU=base2[22,4]
Poblacion2020PU

Poblacion2030PU=((1+vec_iPU)^10)*(Poblacion2020PU)
Poblacion2030PU

hist(Poblacion2030PU)
hist(Poblacion2030PU,main= "Población 2030 Puebla", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030PU)

# EDO. QUERETARO

a=7^5
m=2^31-1
S0QT=200003173+2*22   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0QT=rep(0,n+1)
vec_S0QT[1]=S0QT
for(i in 2:(n+1)){
  vec_S0QT[i]=as.double(a*vec_S0QT[i-1])%%m
}
vec_S0QT=vec_S0QT/m
vec_S0QT<-vec_S0QT[-1]
plot(vec_S0QT)
hist(vec_S0QT)

## SELECCIONAR i

vec_iQT=rep(0,length(vec_S0QT))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0QT[i]<.5) {
    vec_iQT[i]=base2[23,13]
  } else {
    vec_iQT[i]=base2[23,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020QT=base2[23,4]
Poblacion2020QT

Poblacion2030QT=((1+vec_iQT)^10)*(Poblacion2020QT)
Poblacion2030QT

hist(Poblacion2030QT)
hist(Poblacion2030QT,main= "Población 2030 Queretaro", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030QT)

# EDO. QUINTANA ROO

a=7^5
m=2^31-1
S0QR=200003173+2*23   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0QR=rep(0,n+1)
vec_S0QR[1]=S0QR
for(i in 2:(n+1)){
  vec_S0QR[i]=as.double(a*vec_S0QR[i-1])%%m
}
vec_S0QR=vec_S0QR/m
vec_S0QR<-vec_S0QR[-1]
plot(vec_S0QR)
hist(vec_S0QR)

## SELECCIONAR i

vec_iQR=rep(0,length(vec_S0QR))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0QR[i]<.5) {
    vec_iQR[i]=base2[24,13]
  } else {
    vec_iQR[i]=base2[24,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020QR=base2[24,4]
Poblacion2020QR

Poblacion2030QR=((1+vec_iQR)^10)*(Poblacion2020QR)
Poblacion2030QR

hist(Poblacion2030QR)
hist(Poblacion2030QR,main= "Población 2030 Quintana Roo", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030QR)

# EDO. SAN LUIS POTOSI

a=7^5
m=2^31-1
S0SLP=200003173+2*24   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0SLP=rep(0,n+1)
vec_S0SLP[1]=S0SLP
for(i in 2:(n+1)){
  vec_S0SLP[i]=as.double(a*vec_S0SLP[i-1])%%m
}
vec_S0SLP=vec_S0SLP/m
vec_S0SLP<-vec_S0SLP[-1]
plot(vec_S0SLP)
hist(vec_S0SLP)

## SELECCIONAR i

vec_iSLP=rep(0,length(vec_S0SLP))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0SLP[i]<.5) {
    vec_iSLP[i]=base2[25,13]
  } else {
    vec_iSLP[i]=base2[25,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020SLP=base2[25,4]
Poblacion2020SLP

Poblacion2030SLP=((1+vec_iSLP)^10)*(Poblacion2020SLP)
Poblacion2030SLP

hist(Poblacion2030SLP)
hist(Poblacion2030SLP,main= "Población 2030 San Luis Potosí", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030SLP)

# EDO. SINALOA

a=7^5
m=2^31-1
S0SI=200003173+2*25   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0SI=rep(0,n+1)
vec_S0SI[1]=S0SI
for(i in 2:(n+1)){
  vec_S0SI[i]=as.double(a*vec_S0SI[i-1])%%m
}
vec_S0SI=vec_S0SI/m
vec_S0SI<-vec_S0SI[-1]
plot(vec_S0SI)
hist(vec_S0SI)

## SELECCIONAR i

vec_iSI=rep(0,length(vec_S0SI))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0SI[i]<.5) {
    vec_iSI[i]=base2[26,13]
  } else {
    vec_iSI[i]=base2[26,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020SI=base2[26,4]
Poblacion2020SI

Poblacion2030SI=((1+vec_iSI)^10)*(Poblacion2020SI)
Poblacion2030SI

hist(Poblacion2030SI)
hist(Poblacion2030SI,main= "Población 2030 Sinaloa", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030SI)

# EDO. SONORA

a=7^5
m=2^31-1
S0SO=200003173+2*26   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0SO=rep(0,n+1)
vec_S0SO[1]=S0SO
for(i in 2:(n+1)){
  vec_S0SO[i]=as.double(a*vec_S0SO[i-1])%%m
}
vec_S0SO=vec_S0SO/m
vec_S0SO<-vec_S0SO[-1]
plot(vec_S0SO)
hist(vec_S0SO)

## SELECCIONAR i

vec_iSO=rep(0,length(vec_S0SO))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0SO[i]<.5) {
    vec_iSO[i]=base2[27,13]
  } else {
    vec_iSO[i]=base2[27,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020SO=base2[27,4]
Poblacion2020SO

Poblacion2030SO=((1+vec_iSO)^10)*(Poblacion2020SO)
Poblacion2030SO

hist(Poblacion2030SO)
hist(Poblacion2030SO,main= "Población 2030 Sonora", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030SO)

# EDO. TABASCO

a=7^5
m=2^31-1
S0TA=200003173+2*27   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0TA=rep(0,n+1)
vec_S0TA[1]=S0TA
for(i in 2:(n+1)){
  vec_S0TA[i]=as.double(a*vec_S0TA[i-1])%%m
}
vec_S0TA=vec_S0TA/m
vec_S0TA<-vec_S0TA[-1]
plot(vec_S0TA)
hist(vec_S0TA)

## SELECCIONAR i

vec_iTA=rep(0,length(vec_S0TA))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0TA[i]<.5) {
    vec_iTA[i]=base2[28,13]
  } else {
    vec_iTA[i]=base2[28,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020TA=base2[28,4]
Poblacion2020TA

Poblacion2030TA=((1+vec_iTA)^10)*(Poblacion2020TA)
Poblacion2030TA

hist(Poblacion2030TA)
hist(Poblacion2030TA,main= "Población 2030 Tabasco", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030TA)

#EDO. TAMAULIPAS

a=7^5
m=2^31-1
S0TM=200003173+2*28   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0TM=rep(0,n+1)
vec_S0TM[1]=S0TM
for(i in 2:(n+1)){
  vec_S0TM[i]=as.double(a*vec_S0TM[i-1])%%m
}
vec_S0TM=vec_S0TM/m
vec_S0TM<-vec_S0TM[-1]
plot(vec_S0TM)
hist(vec_S0TM)

## SELECCIONAR i

vec_iTM=rep(0,length(vec_S0TM))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0TM[i]<.5) {
    vec_iTM[i]=base2[29,13]
  } else {
    vec_iTM[i]=base2[29,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020TM=base2[29,4]
Poblacion2020TM

Poblacion2030TM=((1+vec_iTM)^10)*(Poblacion2020TM)
Poblacion2030TM

hist(Poblacion2030TM)
hist(Poblacion2030TM,main= "Población 2030 Tamaulipas", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030TM)

# EDO. TLAXCALA

a=7^5
m=2^31-1
S0TX=200003173+2*29   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0TX=rep(0,n+1)
vec_S0TX[1]=S0TX
for(i in 2:(n+1)){
  vec_S0TX[i]=as.double(a*vec_S0TX[i-1])%%m
}
vec_S0TX=vec_S0TX/m
vec_S0TX<-vec_S0TX[-1]
plot(vec_S0TX)
hist(vec_S0TX)

## SELECCIONAR i

vec_iTX=rep(0,length(vec_S0TX))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0TX[i]<.5) {
    vec_iTX[i]=base2[30,13]
  } else {
    vec_iTX[i]=base2[30,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020TX=base2[30,4]
Poblacion2020TX

Poblacion2030TX=((1+vec_iTX)^10)*(Poblacion2020TX)
Poblacion2030TX

hist(Poblacion2030TX)
hist(Poblacion2030TX,main= "Población 2030 Tlaxcala", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030TX)

# EDO. VERACRUZ

a=7^5
m=2^31-1
S0VR=200003173+2*30   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0VR=rep(0,n+1)
vec_S0VR[1]=S0VR
for(i in 2:(n+1)){
  vec_S0VR[i]=as.double(a*vec_S0VR[i-1])%%m
}
vec_S0VR=vec_S0VR/m
vec_S0VR<-vec_S0VR[-1]
plot(vec_S0VR)
hist(vec_S0VR)

## SELECCIONAR i

vec_iVR=rep(0,length(vec_S0VR))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0VR[i]<.5) {
    vec_iVR[i]=base2[31,13]
  } else {
    vec_iVR[i]=base2[31,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020VR=base2[31,4]
Poblacion2020VR

Poblacion2030VR=((1+vec_iVR)^10)*(Poblacion2020VR)
Poblacion2030VR

hist(Poblacion2030VR)
hist(Poblacion2030VR,main= "Población 2030 Veracruz", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030VR)

# EDO. YUCATAN

a=7^5
m=2^31-1
S0YU=200003173+2*31   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0YU=rep(0,n+1)
vec_S0YU[1]=S0YU
for(i in 2:(n+1)){
  vec_S0YU[i]=as.double(a*vec_S0YU[i-1])%%m
}
vec_S0YU=vec_S0YU/m
vec_S0YU<-vec_S0YU[-1]
plot(vec_S0YU)
hist(vec_S0YU)

## SELECCIONAR i

vec_iYU=rep(0,length(vec_S0YU))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0YU[i]<.5) {
    vec_iYU[i]=base2[32,13]
  } else {
    vec_iYU[i]=base2[32,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020YU=base2[32,4]
Poblacion2020YU

Poblacion2030YU=((1+vec_iYU)^10)*(Poblacion2020YU)
Poblacion2030YU

hist(Poblacion2030YU)
hist(Poblacion2030YU,main= "Población 2030 Yucatan", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030YU)

# EDO. ZACATECAS

a=7^5
m=2^31-1
S0ZA=200003173+2*32   # Simulacion i
n=10000

# Simulamos  uniformes y escogemos una para después seleccionar nuestra i

vec_S0ZA=rep(0,n+1)
vec_S0ZA[1]=S0ZA
for(i in 2:(n+1)){
  vec_S0ZA[i]=as.double(a*vec_S0ZA[i-1])%%m
}
vec_S0ZA=vec_S0ZA/m
vec_S0ZA<-vec_S0ZA[-1]
plot(vec_S0ZA)
hist(vec_S0ZA)

## SELECCIONAR i

vec_iZA=rep(0,length(vec_S0ZA))
#for (p in 1:32){}
for(i in 1:n){
  
  if(vec_S0ZA[i]<.5) {
    vec_iZA[i]=base2[33,13]
  } else {
    vec_iZA[i]=base2[33,14]
    
  }}

## ESTIMAR POBLACION 2030

Poblacion2020ZA=base2[33,4]
Poblacion2020ZA

Poblacion2030ZA=((1+vec_iZA)^10)*(Poblacion2020ZA)
Poblacion2030ZA

hist(Poblacion2030ZA)
hist(Poblacion2030ZA,main= "Población 2030 Zacatecas", xlab = "Población Total",ylab = "Frecuencia", col="#8DEEEE")
summary(Poblacion2030ZA)

b<-cbind(c(Poblacion2030AG),c(Poblacion2030BC),c(Poblacion2030BCS),c(Poblacion2030CA),
         c(Poblacion2030CH),c(Poblacion2030CL),c(Poblacion2030CM),c(Poblacion2030CO),
         c(Poblacion2030CS),c(Poblacion2030DG),c(Poblacion2030EM),c(Poblacion2030GRO),
         c(Poblacion2030GTO),c(Poblacion2030HG),c(Poblacion2030JA),c(Poblacion2030MI),
         c(Poblacion2030MO),c(Poblacion2030NA),c(Poblacion2030NL),c(Poblacion2030OA),
         c(Poblacion2030PU),c(Poblacion2030QR),c(Poblacion2030QT),c(Poblacion2030SI),
         c(Poblacion2030SLP),c(Poblacion2030SO),c(Poblacion2030TA),c(Poblacion2030TM),
         c(Poblacion2030TX),c(Poblacion2030VR),c(Poblacion2030YU),c(Poblacion2030ZA))
View(b)

dimnames(b)=list(NULL
                 ,c("Poblacion2030AG","Poblacion2030BC","Poblacion2030BCS","Poblacion2030CA",
                    "Poblacion2030CH","Poblacion2030CL","Poblacion2030CM","Poblacion2030CO",
                    "Poblacion2030CS","Poblacion2030DG","Poblacion2030EM","Poblacion2030GRO",
                    "Poblacion2030GTO","Poblacion2030HG","Poblacion2030JA","Poblacion2030MI",
                    "Poblacion2030MO","Poblacion2030NA","Poblacion2030NL","Poblacion2030OA",
                    "Poblacion2030PU","Poblacion2030QR","Poblacion2030QT","Poblacion2030SI",
                    "Poblacion2030SLP","Poblacion2030SO","Poblacion2030TA","Poblacion2030TM",
                    "Poblacion2030TX","Poblacion2030VR","Poblacion2030YU","Poblacion2030ZA"))

View(b)

## POBLACIÓN TOTAL 2030

PoblacionTotal2030<-b
SumaPoblacion<- rowSums(PoblacionTotal2030)
PoblaciónTotal<- cbind(PoblacionTotal2030,SumaPoblacion)
View(PoblaciónTotal)

hist(rowSums(PoblacionTotal2030), main= "Población Total 2030", xlab = "Población Total", ylab = "Frecuencia", col="#C1FFC1")
summary(SumaPoblacion)



## PRUEBA DE KOLMOGÓROV-SMIRNOV

## AGUAS CALIENTES
AG = matrix(ncol=3,nrow=n)
colnames(AG)=c("Uniformes1", "i/n","Diferencia1")
View(AG)
AG[,1]=sort(vec_S0AG)
cont=0
for (i in 1:n){
  AG[i,2]=i/n
}
AG[,3]=abs(AG[,2]-AG[,1])
plot(AG[,3], main = "Población Aguas Calientes", xlab = "Uniformes", ylab = "Frecuencia")
View(AG)
summary(AG[,3])


ks1=1.22/sqrt(n) #90% de confianza
ks2=1.36/sqrt(n) #95% de confianza
ks3=1.63/sqrt(n) #99% de confianza


## BAJA CALIFORNIA 
BC= matrix(ncol=3,nrow=n)
colnames(BC)=c("Uniformes1", "i/n","Diferencia1")

BC[,1]=sort(vec_S0BC)
cont=0
for (i in 1:n){
  BC[i,2]=i/n
}
BC[,3]=abs(BC[,2]-BC[,1])
plot(BC[,3], main = "Población Baja California", xlab = "Uniformes", ylab = "Frecuencia")
summary(BC[,3])


## BAJA CALIFORNIA SUR
BCS= matrix(ncol=3,nrow=n)
colnames(BCS)=c("Uniformes1", "i/n","Diferencia1")
BCS[,1]=sort(vec_S0BCS)
cont=0
for (i in 1:n){
  BCS[i,2]=i/n
}
BCS[,3]=abs(BCS[,2]-BCS[,1])
plot(BCS[,3], main = "Población Baja California Sur", xlab = "Uniformes", ylab = "Frecuencia")
summary(BCS[,3])


## CAMPECHE
CA= matrix(ncol=3,nrow=n)
colnames(CA)=c("Uniformes1", "i/n","Diferencia1")
CA[,1]=sort(vec_S0CA)
cont=0
for (i in 1:n){
  CA[i,2]=i/n
}
CA[,3]=abs(CA[,2]-CA[,1])
plot(CA[,3], main = "Población Campeche", xlab = "Uniformes", ylab = "Frecuencia")
summary(CA[,3])


## COAHUILA DE ZARAGOZA
CO= matrix(ncol=3,nrow=n)
colnames(CO)=c("Uniformes1", "i/n","Diferencia1")
CO[,1]=sort(vec_S0CO)
cont=0
for (i in 1:n){
  CO[i,2]=i/n
}
CO[,3]=abs(CO[,2]-CO[,1])
plot(CO[,3], main = "Población Coahuila", xlab = "Uniformes", ylab = "Frecuencia")
summary(CO[,3])


## COLIMA
CL= matrix(ncol=3,nrow=n)
colnames(CL)=c("Uniformes1", "i/n","Diferencia1")
CL[,1]=sort(vec_S0CL)
cont=0
for (i in 1:n){
  CL[i,2]=i/n
}
CL[,3]=abs(CL[,2]-CL[,1])
plot(CL[,3], main = "Población Colima", xlab = "Uniformes", ylab = "Frecuencia")
summary(CL[,3])


## CHIAPAS
CS= matrix(ncol=3,nrow=n)
colnames(CS)=c("Uniformes1", "i/n","Diferencia1")
CS[,1]=sort(vec_S0CS)
cont=0
for (i in 1:n){
  CS[i,2]=i/n
}
CS[,3]=abs(CS[,2]-CS[,1])
plot(CS[,3], main = "Población Chiapas", xlab = "Uniformes", ylab = "Frecuencia")
summary(CS[,3])


## CHIHUAHA
CH= matrix(ncol=3,nrow=n)
colnames(CH)=c("Uniformes1", "i/n","Diferencia1")
CH[,1]=sort(vec_S0CH)
cont=0
for (i in 1:n){
  CH[i,2]=i/n
}
CH[,3]=abs(CH[,3]-CH[,1])
plot(CH[,3], main = "Población Chihuahua", xlab = "Uniformes", ylab = "Frecuencia")
summary(CH[,3])


## CIUDAD DE MÉXICO
CM= matrix(ncol=3,nrow=n)
colnames(CM)=c("Uniformes1", "i/n","Diferencia1")
CM[,1]=sort(vec_S0CM)
cont=0
for (i in 1:n){
  CM[i,2]=i/n
}
CM[,3]=abs(CM[,3]-CM[,1])
plot(CM[,3], main = "Población Ciudad de México", xlab = "Uniformes", ylab = "Frecuencia")
summary(CM[,3])


## DURANGO
DG= matrix(ncol=3,nrow=n)
colnames(DG)=c("Uniformes1", "i/n","Diferencia1")
DG[,1]=sort(vec_S0DG)
cont=0
for (i in 1:n){
  DG[i,2]=i/n
}
DG[,3]=abs(DG[,2]-DG[,1])
plot(DG[,3], main = "Población Durango", xlab = "Uniformes", ylab = "Frecuencia")
summary(DG[,3])


## GUANAJUATO
GTO= matrix(ncol=3,nrow=n)
colnames(GTO)=c("Uniformes1", "i/n","Diferencia1")
GTO[,1]=sort(vec_S0GTO)
cont=0
for (i in 1:n){
  GTO[i,2]=i/n
}
GTO[,3]=abs(GTO[,2]-GTO[,1])
plot(GTO[,3], main = "Población Guanajuato", xlab = "Uniformes", ylab = "Frecuencia")
summary(GTO[,3])


## GUERRERO
GRO= matrix(ncol=3,nrow=n)
colnames(GRO)=c("Uniformes1", "i/n","Diferencia1")
GRO[,1]=sort(vec_S0GRO)
cont=0
for (i in 1:n){
  GRO[i,2]=i/n
}
GRO[,3]=abs(GRO[,2]-GRO[,1])
plot(GRO[,3], main = "Población Guerrero", xlab = "Uniformes", ylab = "Frecuencia")
summary(GRO[,3])


## HIDALGO
HG= matrix(ncol=3,nrow=n)
colnames(HG)=c("Uniformes1", "i/n","Diferencia1")
HG[,1]=sort(vec_S0HG)
cont=0
for (i in 1:n){
  HG[i,2]=i/n
}
HG[,3]=abs(HG[,2]-HG[,1])
plot(HG[,3], main = "Población Hidalgo", xlab = "Uniformes", ylab = "Frecuencia")
summary(HG[,3])


## JALISCO
JA= matrix(ncol=3,nrow=n)
colnames(JA)=c("Uniformes1", "i/n","Diferencia1")
JA[,1]=sort(vec_S0JA)
cont=0
for (i in 1:n){
  JA[i,2]=i/n
}
JA[,3]=abs(JA[,2]-JA[,1])
plot(JA[,3], main = "Población Jalisco", xlab = "Uniformes", ylab = "Frecuencia")
summary(JA[,3])


## ESTADO DE MÉXICO
EM= matrix(ncol=3,nrow=n)
colnames(EM)=c("Uniformes1", "i/n","Diferencia1")
EM[,1]=sort(vec_S0EM)
cont=0
for (i in 1:n){
  EM[i,2]=i/n
}
EM[,3]=abs(EM[,2]-EM[,1])
plot(EM[,3], main = "Población Estado de México", xlab = "Uniformes", ylab = "Frecuencia")
summary(EM[,3])


## MICHOACAN
MI= matrix(ncol=3,nrow=n)
colnames(MI)=c("Uniformes1", "i/n","Diferencia1")
MI[,1]=sort(vec_S0MI)
cont=0
for (i in 1:n){
  MI[i,2]=i/n
}
MI[,3]=abs(MI[,2]-MI[,1])
plot(MI[,3], main = "Población Michoacan", xlab = "Uniformes", ylab = "Frecuencia")
summary(MI[,3])


## MORELOS
MO= matrix(ncol=3,nrow=n)
colnames(MO)=c("Uniformes1", "i/n","Diferencia1")
MO[,1]=sort(vec_S0MO)
cont=0
for (i in 1:n){
  MO[i,2]=i/n
}
MO[,3]=abs(MO[,2]-MO[,1])
plot(MO[,3], main = "Población Morelos", xlab = "Uniformes", ylab = "Frecuencia")
summary(MO[,3])


## NAYARIT
NAY= matrix(ncol=3,nrow=n)
colnames(NAY)=c("Uniformes1", "i/n","Diferencia1")
NAY[,1]=sort(vec_S0NA)
cont=0
for (i in 1:n){
  NAY[i,2]=i/n
}
NAY[,3]=abs(NAY[,2]-NAY[,1])
plot(NAY[,3], main = "Población Nayarit", xlab = "Uniformes", ylab = "Frecuencia")
summary(NAY[,3])


## NUEVO LEON
NL= matrix(ncol=3,nrow=n)
colnames(NL)=c("Uniformes1", "i/n","Diferencia1")
NL[,1]=sort(vec_S0NL)
cont=0
for (i in 1:n){
  NL[i,2]=i/n
}
NL[,3]=abs(NL[,2]-NL[,1])
plot(NL[,3], main = "Población Nuevo Leon", xlab = "Uniformes", ylab = "Frecuencia")
summary(NL[,3])


## OAXACA
OA= matrix(ncol=3,nrow=n)
colnames(OA)=c("Uniformes1", "i/n","Diferencia1")
OA[,1]=sort(vec_S0OA)
cont=0
for (i in 1:n){
  OA[i,2]=i/n
}
OA[,3]=abs(OA[,2]-OA[,1])
plot(OA[,3], main = "Población Oaxaca", xlab = "Uniformes", ylab = "Frecuencia")
summary(OA[,3])


## PUEBLA
PU= matrix(ncol=3,nrow=n)
colnames(PU)=c("Uniformes1", "i/n","Diferencia1")
PU[,1]=sort(vec_S0PU)
cont=0
for (i in 1:n){
  PU[i,2]=i/n
}
PU[,3]=abs(PU[,2]-PU[,1])
plot(PU[,3], main = "Población Puebla", xlab = "Uniformes", ylab = "Frecuencia")
summary(PU[,3])


## QUERETARO
QT= matrix(ncol=3,nrow=n)
colnames(QT)=c("Uniformes1", "i/n","Diferencia1")
QT[,1]=sort(vec_S0QT)
cont=0
for (i in 1:n){
  QT[i,2]=i/n
}
QT[,3]=abs(QT[,2]-QT[,1])
plot(QT[,3], main = "Población Queretaro", xlab = "Uniformes", ylab = "Frecuencia")
summary(QT[,3])


## QUINTANA ROO
QR= matrix(ncol=3,nrow=n)
colnames(QR)=c("Uniformes1", "i/n","Diferencia1")
QR[,1]=sort(vec_S0QR)
cont=0
for (i in 1:n){
  QR[i,2]=i/n
}
QR[,3]=abs(QR[,2]-QR[,1])
plot(QR[,3], main = "Población Quintana Roo", xlab = "Uniformes", ylab = "Frecuencia")
summary(QR[,3])


## SAN LUIS POTOSI
SLP= matrix(ncol=3,nrow=n)
colnames(SLP)=c("Uniformes1", "i/n","Diferencia1")
SLP[,1]=sort(vec_S0SLP)
cont=0
for (i in 1:n){
  SLP[i,2]=i/n
}
SLP[,3]=abs(SLP[,2]-SLP[,1])
plot(SLP[,3], main = "Población San Luis Potosi", xlab = "Uniformes", ylab = "Frecuencia")
summary(SLP[,3])


## SINALOA
SI= matrix(ncol=3,nrow=n)
colnames(SI)=c("Uniformes1", "i/n","Diferencia1")
SI[,1]=sort(vec_S0SI)
cont=0
for (i in 1:n){
  SI[i,2]=i/n
}
SI[,3]=abs(SI[,2]-SI[,1])
plot(SI[,3], main = "Población Sinaloa", xlab = "Uniformes", ylab = "Frecuencia")
summary(SI[,3])


## SONORA
SO= matrix(ncol=3,nrow=n)
colnames(SO)=c("Uniformes1", "i/n","Diferencia1")
SO[,1]=sort(vec_S0SO)
cont=0
for (i in 1:n){
  SO[i,2]=i/n
}
SO[,3]=abs(SO[,2]-SO[,1])
plot(SO[,3], main = "Población Sonora", xlab = "Uniformes", ylab = "Frecuencia")
summary(SO[,3])


## TABASCO
TA= matrix(ncol=3,nrow=n)
colnames(TA)=c("Uniformes1", "i/n","Diferencia1")
TA[,1]=sort(vec_S0TA)
cont=0
for (i in 1:n){
  TA[i,2]=i/n
}
TA[,3]=abs(TA[,2]-TA[,1])
plot(TA[,3], main = "Población Tabasco", xlab = "Uniformes", ylab = "Frecuencia")
summary(TA[,3])


## TAMAULIPAS
TM= matrix(ncol=3,nrow=n)
colnames(TM)=c("Uniformes1", "i/n","Diferencia1")
TM[,1]=sort(vec_S0TM)
cont=0
for (i in 1:n){
  TM[i,2]=i/n
}
TM[,3]=abs(TM[,2]-TM[,1])
plot(TM[,3], main = "Población Tamaulipas", xlab = "Uniformes", ylab = "Frecuencia")
summary(TM[,3])


## TLAXCALA
TX= matrix(ncol=3,nrow=n)
colnames(TX)=c("Uniformes1", "i/n","Diferencia1")
TX[,1]=sort(vec_S0TX)
cont=0
for (i in 1:n){
  TX[i,2]=i/n
}
TX[,3]=abs(TX[,2]-TX[,1])
plot(TX[,3], main = "Población Tlaxcala", xlab = "Uniformes", ylab = "Frecuencia")
summary(TX[,3])


## VERACRUZ
VR= matrix(ncol=3,nrow=n)
colnames(VR)=c("Uniformes1", "i/n","Diferencia1")
VR[,1]=sort(vec_S0VR)
cont=0
for (i in 1:n){
  VR[i,2]=i/n
}
VR[,3]=abs(VR[,2]-VR[,1])
plot(VR[,3], main = "Población Veracruz", xlab = "Uniformes", ylab = "Frecuencia")
summary(VR[,3])


## YUCATAN
YU= matrix(ncol=3,nrow=n)
colnames(YU)=c("Uniformes1", "i/n","Diferencia1")
YU[,1]=sort(vec_S0YU)
cont=0
for (i in 1:n){
  YU[i,2]=i/n
}
YU[,3]=abs(YU[,2]-YU[,1])
plot(YU[,3], main = "Población Yucatan", xlab = "Uniformes", ylab = "Frecuencia")
summary(YU[,3])


## ZACATECAS
ZA= matrix(ncol=3,nrow=n)
colnames(ZA)=c("Uniformes1", "i/n","Diferencia1")
ZA[,1]=sort(vec_S0ZA)
cont=0
for (i in 1:n){
  ZA[i,2]=i/n
}
ZA[,3]=abs(ZA[,2]-ZA[,1])
plot(ZA[,3], main = "Población Zacatecas", xlab = "Uniformes", ylab = "Frecuencia")
summary(ZA[,3])
