library(tidyverse)
library(elsoc)


### Sexo recodificacion #####

elsoc_wide_2016_2021$m0_sexo_w01_fac <- factor(elsoc_wide_2016_2021$m0_sexo_w01,
                             labels = c("Hombre","Mujer"),
                             levels = c(1,2)) 

elsoc_wide_2016_2021$m0_sexo_w02_fac <- factor(elsoc_wide_2016_2021$m0_sexo_w02,
                                               labels = c("Hombre","Mujer"),
                                               levels = c(1,2)) 

elsoc_wide_2016_2021$m0_sexo_w03_fac <- factor(elsoc_wide_2016_2021$m0_sexo_w03,
                                               labels = c("Hombre","Mujer"),
                                               levels = c(1,2)) 

elsoc_wide_2016_2021$m0_sexo_w04_fac <- factor(elsoc_wide_2016_2021$m0_sexo_w04,
                                               labels = c("Hombre","Mujer"),
                                               levels = c(1,2)) 

elsoc_wide_2016_2021$m0_sexo_w05_fac <- factor(elsoc_wide_2016_2021$m0_sexo_w05,
                                               labels = c("Hombre","Mujer"),
                                               levels = c(1,2)) 


##### Edad ###################


## EDAD OLA 1####
elsoc_wide_2016_2021$m0_edad_w01_fac <- as.character(car::recode(elsoc_wide_2016_2021$m0_edad_w01, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2016_2021$m0_edad_w01_fac[elsoc_wide_2016_2021$m0_edad_w01_fac==1]="18 a 30 años"
elsoc_wide_2016_2021$m0_edad_w01_fac[elsoc_wide_2016_2021$m0_edad_w01_fac==2]="31 a 50 años"
elsoc_wide_2016_2021$m0_edad_w01_fac[elsoc_wide_2016_2021$m0_edad_w01_fac==3]="51 a 70 años"
elsoc_wide_2016_2021$m0_edad_w01_fac[elsoc_wide_2016_2021$m0_edad_w01_fac==4]="71 o más años"

## OLA 2 #############

elsoc_wide_2016_2021$m0_edad_w02_fac <- as.character(car::recode(elsoc_wide_2016_2021$m0_edad_w02, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2016_2021$m0_edad_w02_fac[elsoc_wide_2016_2021$m0_edad_w02_fac==1]="18 a 30 años"
elsoc_wide_2016_2021$m0_edad_w02_fac[elsoc_wide_2016_2021$m0_edad_w02_fac==2]="31 a 50 años"
elsoc_wide_2016_2021$m0_edad_w02_fac[elsoc_wide_2016_2021$m0_edad_w02_fac==3]="51 a 70 años"
elsoc_wide_2016_2021$m0_edad_w02_fac[elsoc_wide_2016_2021$m0_edad_w02_fac==4]="71 o más años"

### OLA 3 ##############

elsoc_wide_2016_2021$m0_edad_w03_fac <- as.character(car::recode(elsoc_wide_2016_2021$m0_edad_w03, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2016_2021$m0_edad_w03_fac[elsoc_wide_2016_2021$m0_edad_w03_fac==1]="18 a 30 años"
elsoc_wide_2016_2021$m0_edad_w03_fac[elsoc_wide_2016_2021$m0_edad_w03_fac==2]="31 a 50 años"
elsoc_wide_2016_2021$m0_edad_w03_fac[elsoc_wide_2016_2021$m0_edad_w03_fac==3]="51 a 70 años"
elsoc_wide_2016_2021$m0_edad_w03_fac[elsoc_wide_2016_2021$m0_edad_w03_fac==4]="71 o más años"

##### OLA 4 ##########


elsoc_wide_2016_2021$m0_edad_w04_fac <- as.character(car::recode(elsoc_wide_2016_2021$m0_edad_w04, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2016_2021$m0_edad_w04_fac[elsoc_wide_2016_2021$m0_edad_w04_fac==1]="18 a 30 años"
elsoc_wide_2016_2021$m0_edad_w04_fac[elsoc_wide_2016_2021$m0_edad_w04_fac==2]="31 a 50 años"
elsoc_wide_2016_2021$m0_edad_w04_fac[elsoc_wide_2016_2021$m0_edad_w04_fac==3]="51 a 70 años"
elsoc_wide_2016_2021$m0_edad_w04_fac[elsoc_wide_2016_2021$m0_edad_w04_fac==4]="71 o más años"

####### OLA 5 ######################


elsoc_wide_2016_2021$m0_edad_w05_fac <- as.character(car::recode(elsoc_wide_2016_2021$m0_edad_w05, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2016_2021$m0_edad_w05_fac[elsoc_wide_2016_2021$m0_edad_w05_fac==1]="18 a 30 años"
elsoc_wide_2016_2021$m0_edad_w05_fac[elsoc_wide_2016_2021$m0_edad_w05_fac==2]="31 a 50 años"
elsoc_wide_2016_2021$m0_edad_w05_fac[elsoc_wide_2016_2021$m0_edad_w05_fac==3]="51 a 70 años"
elsoc_wide_2016_2021$m0_edad_w05_fac[elsoc_wide_2016_2021$m0_edad_w05_fac==4]="71 o más años"

############################# NIVEL EDUCACION ###############

# ola 1 #

elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==1]="Sin estudios"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==2]="Basica Incompleta"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==3]="Basica Completa"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==4]="Media Incompleta"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==5]="Media Completa"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==6]="Tecnica Incompleta"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==7]="Tecnica Completa"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==8]="Universitaria Incompleta"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==9]="Universitaria Completa"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==10]="Postgrado"
elsoc_wide_2016_2021$m01_w01_fac[elsoc_wide_2016_2021$m01_w01==-999]=NA

# ola 2 #

elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==1]="Sin estudios"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==2]="Basica Incompleta"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==3]="Basica Completa"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==4]="Media Incompleta"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==5]="Media Completa"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==6]="Tecnica Incompleta"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==7]="Tecnica Completa"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==8]="Universitaria Incompleta"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==9]="Universitaria Completa"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==10]="Postgrado"
elsoc_wide_2016_2021$m01_w02_fac[elsoc_wide_2016_2021$m01_w02==-999]=NA

## Ola 3 ################

elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==1]="Sin estudios"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==2]="Basica Incompleta"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==3]="Basica Completa"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==4]="Media Incompleta"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==5]="Media Completa"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==6]="Tecnica Incompleta"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==7]="Tecnica Completa"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==8]="Universitaria Incompleta"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==9]="Universitaria Completa"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==10]="Postgrado"
elsoc_wide_2016_2021$m01_w03_fac[elsoc_wide_2016_2021$m01_w03==-999]=NA

########## ola 4 #################

elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==1]="Sin estudios"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==2]="Basica Incompleta"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==3]="Basica Completa"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==4]="Media Incompleta"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==5]="Media Completa"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==6]="Tecnica Incompleta"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==7]="Tecnica Completa"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==8]="Universitaria Incompleta"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==9]="Universitaria Completa"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==10]="Postgrado"
elsoc_wide_2016_2021$m01_w04_fac[elsoc_wide_2016_2021$m01_w04==-999]=NA

################ ola 5 ################

elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==1]="Sin estudios"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==2]="Basica Incompleta"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==3]="Basica Completa"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==4]="Media Incompleta"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==5]="Media Completa"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==6]="Tecnica Incompleta"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==7]="Tecnica Completa"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==8]="Universitaria Incompleta"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==9]="Universitaria Completa"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==10]="Postgrado"
elsoc_wide_2016_2021$m01_w05_fac[elsoc_wide_2016_2021$m01_w05==-999]=NA


##################### Ingresos Mensuales ##################

# COnsultar con nati
unique(elsoc_wide_2016_2021$m14b_w05)

elsoc_wide_2016_2021$m14_w01_fac <- as.character(car::recode(elsoc_wide_2016_2021$m14_w01, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


########## NIVEL EDUCACIONAL DEL PADRE ##########

