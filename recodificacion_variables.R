library(tidyverse)
library(elsoc)
library(summarytools)
library(matrixStats)

load("ELSOC_Wide_2016_2021_v1.00_R.RData")

elsoc_wide_2018_2021  <- elsoc_wide_2016_2021 %>% 
  filter(tipo_atricion == 1 | tipo_atricion == 9| tipo_atricion ==17 )


elsoc_wide_2018_2021 <- elsoc_wide_2018_2021 %>% car::recode(., "c(-888,-999)=NA")

attach(elsoc_wide_2018_2021)

#### Grado de confianza en el Gobierno

label(elsoc_wide_2018_2021$c05_01_w03) <- "Confianza en el gobierno (2018)."
label(elsoc_wide_2018_2021$c05_01_w04) <- "Confianza en el gobierno (2019)."
label(elsoc_wide_2018_2021$c05_01_w05) <- "Confianza en el gobierno (2020)."


#### Grado de Confianza en los Partidos Políticos


label(c05_02_w03) <- "Confianza en los partidos politicos (2018)."
label(c05_02_w04) <- "Confianza en los partidos politicos (2019)."
label(c05_02_w05) <- "Confianza en los partidos politicos (2020)."


#### Grado de Confianza en el Presidente/a de la República

label(c05_08_w03) <- "Confianza en el Presidente/a de la Republica (2018)."
label(c05_08_w04) <- "Confianza en el Presidente/a de la Republica (2019)."
label(c05_08_w05) <- "Confianza en el Presidente/a de la Republica (2020)."

#### Cálculo de Confianza Insticional 

elsoc_wide_2018_2021 <- elsoc_wide_2018_2021 %>%
  rowwise() %>%
  mutate(conf_institucional_w03 =
           mean(c(c05_01_w03, c05_02_w03, c05_08_w03), na.rm = T))

elsoc_wide_2018_2021 <- elsoc_wide_2018_2021 %>%
  rowwise() %>%
  mutate(conf_institucional_w04 =
           mean(c(c05_01_w04, c05_02_w04, c05_08_w04), na.rm = T))

elsoc_wide_2018_2021 <- elsoc_wide_2018_2021 %>%
  rowwise() %>%
  mutate(conf_institucional_w05 =
           mean(c(c05_01_w05, c05_08_w05, c05_02_w05), na.rm = T))

elsoc_wide_2018_2021$conf_institucional_w03 <- ifelse((elsoc_wide_2018_2021$conf_institucional_w03<1.5), 1,
                                      ifelse((elsoc_wide_2018_2021$conf_institucional_w03<2.5), 2,
                                             ifelse((elsoc_wide_2018_2021$conf_institucional_w03<3.5), 3,
                                                    ifelse((elsoc_wide_2018_2021$conf_institucional_w03<4.5), 4,
                                                           ifelse((elsoc_wide_2018_2021$conf_institucional_w03<=5), 5, NA)))))
elsoc_wide_2018_2021$conf_institucional_w04 <- ifelse((elsoc_wide_2018_2021$conf_institucional_w04<1.5), 1,
                                      ifelse((elsoc_wide_2018_2021$conf_institucional_w04<2.5), 2,
                                             ifelse((elsoc_wide_2018_2021$conf_institucional_w04<3.5), 3,
                                                    ifelse((elsoc_wide_2018_2021$conf_institucional_w04<4.5), 4,
                                                           ifelse((elsoc_wide_2018_2021$conf_institucional_w04<=5), 5, NA)))))
elsoc_wide_2018_2021$conf_institucional_w05 <- ifelse((elsoc_wide_2018_2021$conf_institucional_w05<1.5), 1,
                                      ifelse((elsoc_wide_2018_2021$conf_institucional_w05<2.5), 2,
                                             ifelse((elsoc_wide_2018_2021$conf_institucional_w05<3.5), 3,
                                                    ifelse((elsoc_wide_2018_2021$conf_institucional_w05<4.5), 4,
                                                           ifelse((elsoc_wide_2018_2021$conf_institucional_w05<=5), 5, NA)))))

elsoc_wide_2018_2021$conf_institucional_w03 <- car::recode(elsoc_wide_2018_2021$conf_institucional_w03, "c(1,2)=1; 3=2; c(4,5)=3")
elsoc_wide_2018_2021$conf_institucional_w04 <- car::recode(elsoc_wide_2018_2021$conf_institucional_w04, "c(1,2)=1; 3=2; c(4,5)=3")
elsoc_wide_2018_2021$conf_institucional_w05 <- car::recode(elsoc_wide_2018_2021$conf_institucional_w05, "c(1,2)=1; 3=2; c(4,5)=3")


elsoc_wide_2018_2021$conf_institucional_w03<- factor(elsoc_wide_2018_2021$conf_institucional_w03, levels = c(1, 2, 3), labels = c("Nada o Poca", "Algo", "Bastante o Mucha"))   
elsoc_wide_2018_2021$conf_institucional_w04<- factor(elsoc_wide_2018_2021$conf_institucional_w04, levels = c(1, 2, 3), labels = c("Nada o Poca", "Algo", "Bastante o Mucha"))   
elsoc_wide_2018_2021$conf_institucional_w05<- factor(elsoc_wide_2018_2021$conf_institucional_w05, levels = c(1, 2, 3), labels = c("Nada o Poca", "Algo", "Bastante o Mucha"))   


#### Percepción de la Desigualdad
?rowWeightedMeans

label(c18_11_w03) <- "Grado de acuerdo: Las diferencias de ingreso son demasiado grandes (2018)."
label(c18_11_w04) <- "Grado de acuerdo: Las diferencias de ingreso son demasiado grandes (2019)."
label(c18_11_w05) <- "Grado de acuerdo: Las diferencias de ingreso son demasiado grandes (2020)."

### Preferencias Distributivas

label(d02_01_w03) <- "Grado de acuerdo: Justicia distributiva en pensiones (2018)."
label(d02_01_w04) <- "Grado de acuerdo: Justicia distributiva en pensiones (2019)."
# label(d02_01_w05) <- "Grado de acuerdo: Justicia distributiva en pensiones (2020)." ### Esta variable no fue medida en el 2020

label(d02_02_w03) <- "Grado de acuerdo: Justicia distributiva en educacion (2018)."
label(d02_02_w04) <- "Grado de acuerdo: Justicia distributiva en educacion (2019)."
# label(d02_02_w05) <- "Grado de acuerdo: Justicia distributiva en educacion (2020)." ### Esta variable no fue medida en el 2020

label(d02_03_w03) <- "Grado de acuerdo: Justicia distributiva en salud (2018)."
label(d02_03_w04) <- "Grado de acuerdo: Justicia distributiva en salud (2019)."
# label(d02_03_w05) <- "Grado de acuerdo: Justicia distributiva en salud (2020)." ### Esta variable no fue medida en el 2020

### Percepción de Movilidad Social [Bateria de preguntas Surgir en la vida]

label(d05_01_w03) <- "Grado de importancia: Provenir de una familia adinerada (2018)."
label(d05_01_w04) <- "Grado de importancia: Provenir de una familia adinerada (2019)."
label(d05_01_w05) <- "Grado de importancia: Provenir de una familia adinerada (2020)."

label(d05_02_w03) <- "Grado de importancia: Tener un buen nivel de educacion (2018)."
label(d05_02_w04) <- "Grado de importancia: Tener un buen nivel de educacion (2019)."
label(d05_02_w05) <- "Grado de importancia: Tener un buen nivel de educacion (2020)."

label(d05_03_w03) <- "Grado de importancia: Tener ambicion (2018)."
label(d05_03_w04) <- "Grado de importancia: Tener ambicion (2019)."
label(d05_03_w05) <- "Grado de importancia: Tener ambicion (2020)."

#### Cálculo de Percepción de Movilidad Social Ponderada

elsoc_wide_2018_2021 <- elsoc_wide_2018_2021 %>%
  rowwise() %>%
  mutate(per_mov_social_w03 =
           mean(c(d05_01_w03, d05_02_w03, d05_03_w03), na.rm = T))

elsoc_wide_2018_2021 <- elsoc_wide_2018_2021 %>%
  rowwise() %>%
  mutate(per_mov_social_w04 =
           mean(c(d05_01_w04, d05_02_w04, d05_03_w04), na.rm = T))

elsoc_wide_2018_2021 <- elsoc_wide_2018_2021 %>%
  rowwise() %>%
  mutate(per_mov_social_w05 =
           mean(c(d05_01_w05, d05_02_w05, d05_03_w05), na.rm = T))

### Expectativas de Movilidad Social [Status Social Subjetivo]

label(d01_01_w03) <- "Estatus Social Subjetivo: Donde se ubicaria ud. en la sociedad chilena (2018)."
label(d01_01_w04) <- "Estatus Social Subjetivo: Donde se ubicaria ud. en la sociedad chilena (2019)."
label(d01_01_w05) <- "Estatus Social Subjetivo: Donde se ubicaria ud. en la sociedad chilena (2020)."

label(d01_03_w03) <- "Estatus Social Subjetivo: Donde se ubicarian sus hijos (2018)."
label(d01_03_w04) <- "Estatus Social Subjetivo: Donde se ubicarian sus hijos (2019)."
label(d01_03_w05) <- "Estatus Social Subjetivo: Donde se ubicarian sus hijos (2020)."

elsoc_wide_2018_2021$exp_mov_social_w03 <- 
### Sexo recodificacion #####


elsoc_wide_2018_2021$m0_sexo_w03_fac <- factor(elsoc_wide_2018_2021$m0_sexo_w03,
                                               labels = c("Hombre","Mujer"),
                                               levels = c(1,2)) 


elsoc_wide_2018_2021$m0_sexo_w04_fac <- factor(elsoc_wide_2018_2021$m0_sexo_w04,
                                               labels = c("Hombre","Mujer"),
                                               levels = c(1,2)) 


elsoc_wide_2018_2021$m0_sexo_w05_fac <- factor(elsoc_wide_2018_2021$m0_sexo_w05,
                                               labels = c("Hombre","Mujer"),
                                               levels = c(1,2)) 


##### Edad ###################


## EDAD OLA 1####
elsoc_wide_2018_2021$m0_edad_w01_fac <- as.character(car::recode(elsoc_wide_2018_2021$m0_edad_w01, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2018_2021$m0_edad_w01_fac[elsoc_wide_2018_2021$m0_edad_w01_fac==1]="18 a 30 años"
elsoc_wide_2018_2021$m0_edad_w01_fac[elsoc_wide_2018_2021$m0_edad_w01_fac==2]="31 a 50 años"
elsoc_wide_2018_2021$m0_edad_w01_fac[elsoc_wide_2018_2021$m0_edad_w01_fac==3]="51 a 70 años"
elsoc_wide_2018_2021$m0_edad_w01_fac[elsoc_wide_2018_2021$m0_edad_w01_fac==4]="71 o más años"

## OLA 2 #############

elsoc_wide_2018_2021$m0_edad_w02_fac <- as.character(car::recode(elsoc_wide_2018_2021$m0_edad_w02, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2018_2021$m0_edad_w02_fac[elsoc_wide_2018_2021$m0_edad_w02_fac==1]="18 a 30 años"
elsoc_wide_2018_2021$m0_edad_w02_fac[elsoc_wide_2018_2021$m0_edad_w02_fac==2]="31 a 50 años"
elsoc_wide_2018_2021$m0_edad_w02_fac[elsoc_wide_2018_2021$m0_edad_w02_fac==3]="51 a 70 años"
elsoc_wide_2018_2021$m0_edad_w02_fac[elsoc_wide_2018_2021$m0_edad_w02_fac==4]="71 o más años"

### OLA 3 ##############

elsoc_wide_2018_2021$m0_edad_w03_fac <- as.character(car::recode(elsoc_wide_2018_2021$m0_edad_w03, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2018_2021$m0_edad_w03_fac[elsoc_wide_2018_2021$m0_edad_w03_fac==1]="18 a 30 años"
elsoc_wide_2018_2021$m0_edad_w03_fac[elsoc_wide_2018_2021$m0_edad_w03_fac==2]="31 a 50 años"
elsoc_wide_2018_2021$m0_edad_w03_fac[elsoc_wide_2018_2021$m0_edad_w03_fac==3]="51 a 70 años"
elsoc_wide_2018_2021$m0_edad_w03_fac[elsoc_wide_2018_2021$m0_edad_w03_fac==4]="71 o más años"

##### OLA 4 ##########


elsoc_wide_2018_2021$m0_edad_w04_fac <- as.character(car::recode(elsoc_wide_2018_2021$m0_edad_w04, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2018_2021$m0_edad_w04_fac[elsoc_wide_2018_2021$m0_edad_w04_fac==1]="18 a 30 años"
elsoc_wide_2018_2021$m0_edad_w04_fac[elsoc_wide_2018_2021$m0_edad_w04_fac==2]="31 a 50 años"
elsoc_wide_2018_2021$m0_edad_w04_fac[elsoc_wide_2018_2021$m0_edad_w04_fac==3]="51 a 70 años"
elsoc_wide_2018_2021$m0_edad_w04_fac[elsoc_wide_2018_2021$m0_edad_w04_fac==4]="71 o más años"

####### OLA 5 ######################


elsoc_wide_2018_2021$m0_edad_w05_fac <- as.character(car::recode(elsoc_wide_2018_2021$m0_edad_w05, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


elsoc_wide_2018_2021$m0_edad_w05_fac[elsoc_wide_2018_2021$m0_edad_w05_fac==1]="18 a 30 años"
elsoc_wide_2018_2021$m0_edad_w05_fac[elsoc_wide_2018_2021$m0_edad_w05_fac==2]="31 a 50 años"
elsoc_wide_2018_2021$m0_edad_w05_fac[elsoc_wide_2018_2021$m0_edad_w05_fac==3]="51 a 70 años"
elsoc_wide_2018_2021$m0_edad_w05_fac[elsoc_wide_2018_2021$m0_edad_w05_fac==4]="71 o más años"

############################# NIVEL EDUCACION ###############

# ola 1 #

elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==1]="Sin estudios"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==2]="Basica Incompleta"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==3]="Basica Completa"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==4]="Media Incompleta"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==5]="Media Completa"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==7]="Tecnica Completa"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==9]="Universitaria Completa"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==10]="Postgrado"
elsoc_wide_2018_2021$m01_w01_fac[elsoc_wide_2018_2021$m01_w01==-999]=NA

# ola 2 #

elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==1]="Sin estudios"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==2]="Basica Incompleta"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==3]="Basica Completa"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==4]="Media Incompleta"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==5]="Media Completa"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==7]="Tecnica Completa"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==9]="Universitaria Completa"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==10]="Postgrado"
elsoc_wide_2018_2021$m01_w02_fac[elsoc_wide_2018_2021$m01_w02==-999]=NA

## Ola 3 ################

elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==1]="Menos que media completa"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==2]="Menos que media completa"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==3]="Menos que media completa"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==4]="Menos que media completa"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==5]="Media completa"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==6]="Educacion tecnica superior"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==7]="Educacion tecnica superior"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==8]="Educacion universitaria y Postgrado"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==9]="Educacion universitaria y Postgrado"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==10]="Educacion universitaria y Postgrado"
elsoc_wide_2018_2021$m01_w03_fac[elsoc_wide_2018_2021$m01_w03==-999]=NA

########## ola 4 #################

elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==1]="Sin estudios"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==2]="Basica Incompleta"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==3]="Basica Completa"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==4]="Media Incompleta"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==5]="Media Completa"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==7]="Tecnica Completa"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==9]="Universitaria Completa"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==10]="Postgrado"
elsoc_wide_2018_2021$m01_w04_fac[elsoc_wide_2018_2021$m01_w04==-999]=NA

################ ola 5 ################

elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==1]="Sin estudios"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==2]="Basica Incompleta"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==3]="Basica Completa"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==4]="Media Incompleta"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==5]="Media Completa"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==7]="Tecnica Completa"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==9]="Universitaria Completa"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==10]="Postgrado"
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==-999]=NA
elsoc_wide_2018_2021$m01_w05_fac[elsoc_wide_2018_2021$m01_w05==-888]=NA


## Ola 3 ###########


##################### Ingresos Mensuales ##################

########### ola 1 #####################
elsoc_wide_2018_2021$m13_w01_fac <- as.character(car::recode(elsoc_wide_2018_2021$m13_w01, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


elsoc_wide_2018_2021$m13_w01_fac[elsoc_wide_2018_2021$m13_w01_fac==1]="Menos de 250.000"
elsoc_wide_2018_2021$m13_w01_fac[elsoc_wide_2018_2021$m13_w01_fac==2]="Entre 250.000 a 350.000"
elsoc_wide_2018_2021$m13_w01_fac[elsoc_wide_2018_2021$m13_w01_fac==3]="Entre 350.000 a 450.000"
elsoc_wide_2018_2021$m13_w01_fac[elsoc_wide_2018_2021$m13_w01_fac==4]="Entre 450.000 a 700.000"
elsoc_wide_2018_2021$m13_w01_fac[elsoc_wide_2018_2021$m13_w01_fac==5]="Mas de 700.000"
elsoc_wide_2018_2021$m13_w01_fac[elsoc_wide_2018_2021$m13_w01_fac==-999]=NA
elsoc_wide_2018_2021$m13_w01_fac[elsoc_wide_2018_2021$m13_w01_fac==-888]=NA

unique(elsoc_wide_2018_2021$m13_w01_fac)

## ola 2 ######

elsoc_wide_2018_2021$m13_w02_fac <- as.character(car::recode(elsoc_wide_2018_2021$m13_w02, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


elsoc_wide_2018_2021$m13_w02_fac[elsoc_wide_2018_2021$m13_w02_fac==1]="Menos de 250.000"
elsoc_wide_2018_2021$m13_w02_fac[elsoc_wide_2018_2021$m13_w02_fac==2]="Entre 250.000 a 350.000"
elsoc_wide_2018_2021$m13_w02_fac[elsoc_wide_2018_2021$m13_w02_fac==3]="Entre 350.000 a 450.000"
elsoc_wide_2018_2021$m13_w02_fac[elsoc_wide_2018_2021$m13_w02_fac==4]="Entre 450.000 a 700.000"
elsoc_wide_2018_2021$m13_w02_fac[elsoc_wide_2018_2021$m13_w02_fac==5]="Mas de 700.000"
elsoc_wide_2018_2021$m13_w02_fac[elsoc_wide_2018_2021$m13_w02_fac==-999]=NA
elsoc_wide_2018_2021$m13_w02_fac[elsoc_wide_2018_2021$m13_w02_fac==-888]=NA

### ola 3 ##########

elsoc_wide_2018_2021$m13_w03_fac <- as.character(car::recode(elsoc_wide_2018_2021$m13_w03, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


elsoc_wide_2018_2021$m13_w03_fac[elsoc_wide_2018_2021$m13_w03_fac==1]="Menos de 250.000"
elsoc_wide_2018_2021$m13_w03_fac[elsoc_wide_2018_2021$m13_w03_fac==2]="Entre 250.000 a 350.000"
elsoc_wide_2018_2021$m13_w03_fac[elsoc_wide_2018_2021$m13_w03_fac==3]="Entre 350.000 a 450.000"
elsoc_wide_2018_2021$m13_w03_fac[elsoc_wide_2018_2021$m13_w03_fac==4]="Entre 450.000 a 700.000"
elsoc_wide_2018_2021$m13_w03_fac[elsoc_wide_2018_2021$m13_w03_fac==5]="Mas de 700.000"
elsoc_wide_2018_2021$m13_w03_fac[elsoc_wide_2018_2021$m13_w03_fac==-999]=NA
elsoc_wide_2018_2021$m13_w03_fac[elsoc_wide_2018_2021$m13_w03_fac==-888]=NA


#### ola 4 #############
elsoc_wide_2018_2021$m13_w04_fac <- as.character(car::recode(elsoc_wide_2018_2021$m13_w04, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


elsoc_wide_2018_2021$m13_w04_fac[elsoc_wide_2018_2021$m13_w04_fac==1]="Menos de 250.000"
elsoc_wide_2018_2021$m13_w04_fac[elsoc_wide_2018_2021$m13_w04_fac==2]="Entre 250.000 a 350.000"
elsoc_wide_2018_2021$m13_w04_fac[elsoc_wide_2018_2021$m13_w04_fac==3]="Entre 350.000 a 450.000"
elsoc_wide_2018_2021$m13_w04_fac[elsoc_wide_2018_2021$m13_w04_fac==4]="Entre 450.000 a 700.000"
elsoc_wide_2018_2021$m13_w04_fac[elsoc_wide_2018_2021$m13_w04_fac==5]="Mas de 700.000"
elsoc_wide_2018_2021$m13_w04_fac[elsoc_wide_2018_2021$m13_w04_fac==-999]=NA
elsoc_wide_2018_2021$m13_w04_fac[elsoc_wide_2018_2021$m13_w04_fac==-888]=NA

####### ola 5 ##########

elsoc_wide_2018_2021$m13_w05_fac <- as.character(car::recode(elsoc_wide_2018_2021$m13_w05, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


elsoc_wide_2018_2021$m13_w05_fac[elsoc_wide_2018_2021$m13_w05_fac==1]="Menos de 250.000"
elsoc_wide_2018_2021$m13_w05_fac[elsoc_wide_2018_2021$m13_w05_fac==2]="Entre 250.000 a 350.000"
elsoc_wide_2018_2021$m13_w05_fac[elsoc_wide_2018_2021$m13_w05_fac==3]="Entre 350.000 a 450.000"
elsoc_wide_2018_2021$m13_w05_fac[elsoc_wide_2018_2021$m13_w05_fac==4]="Entre 450.000 a 700.000"
elsoc_wide_2018_2021$m13_w05_fac[elsoc_wide_2018_2021$m13_w05_fac==5]="Mas de 700.000"
elsoc_wide_2018_2021$m13_w05_fac[elsoc_wide_2018_2021$m13_w05_fac==-999]=NA
elsoc_wide_2018_2021$m13_w05_fac[elsoc_wide_2018_2021$m13_w05_fac==-888]=NA


########## NIVEL EDUCACIONAL DEL PADRE ##########
# Nota: estan solo para la ola 1, 3 y 5


### ola 1 ############

elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==1]="Sin estudios"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==2]="Basica Incompleta"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==3]="Basica Completa"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==4]="Media Incompleta"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==5]="Media Completa"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==7]="Tecnica Completa"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==9]="Universitaria Completa"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==10]="Postgrado"
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==-999]=NA
elsoc_wide_2018_2021$m27_w01_fac[elsoc_wide_2018_2021$m27_w01==-888]=NA


############# ola 3 #################

elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==1]="Sin estudios"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==2]="Basica Incompleta"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==3]="Basica Completa"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==4]="Media Incompleta"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==5]="Media Completa"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==7]="Tecnica Completa"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==9]="Universitaria Completa"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==10]="Postgrado"
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==-999]=NA
elsoc_wide_2018_2021$m27_w03_fac[elsoc_wide_2018_2021$m27_w03==-888]=NA

################ ola 5 ##########################

elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==1]="Sin estudios"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==2]="Basica Incompleta"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==3]="Basica Completa"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==4]="Media Incompleta"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==5]="Media Completa"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==7]="Tecnica Completa"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==9]="Universitaria Completa"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==10]="Postgrado"
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==-999]=NA
elsoc_wide_2018_2021$m27_w05_fac[elsoc_wide_2018_2021$m27_w05==-888]=NA


#################### Nivel educacional de la madre ###############
# Nota: solo disponibles para la ola 1, 3 y 5 

####### Ola 1 ################

elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==1]="Sin estudios"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==2]="Basica Incompleta"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==3]="Basica Completa"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==4]="Media Incompleta"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==5]="Media Completa"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==7]="Tecnica Completa"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==9]="Universitaria Completa"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==10]="Postgrado"
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==-999]=NA
elsoc_wide_2018_2021$m28_w01_fac[elsoc_wide_2018_2021$m28_w01==-888]=NA

######## ola 3 #####################

elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==1]="Sin estudios"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==2]="Basica Incompleta"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==3]="Basica Completa"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==4]="Media Incompleta"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==5]="Media Completa"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==7]="Tecnica Completa"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==9]="Universitaria Completa"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==10]="Postgrado"
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==-999]=NA
elsoc_wide_2018_2021$m28_w03_fac[elsoc_wide_2018_2021$m28_w03==-888]=NA

########### ola 5 #######################

elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==1]="Sin estudios"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==2]="Basica Incompleta"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==3]="Basica Completa"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==4]="Media Incompleta"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==5]="Media Completa"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==6]="Tecnica Incompleta"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==7]="Tecnica Completa"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==8]="Universitaria Incompleta"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==9]="Universitaria Completa"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==10]="Postgrado"
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==-999]=NA
elsoc_wide_2018_2021$m28_w05_fac[elsoc_wide_2018_2021$m28_w05==-888]=NA

################  TRABAJO  ########################

#### ola 1 ###########
unique(elsoc_wide_2018_2021$m02_w01_fac)

elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==1]="Trabaja Jornada Completa"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==2]="Trabaja Jornada Parcial"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==3]="Estudia y trabaja"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==4]="Solo estudia"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==5]="Jubilado"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==6]="Desempleado"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==7]="Tareas no remuneradas"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==8]="Enfermedad incapacitante"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==9]="No trabaja ni busca trabajo"
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==-999]=NA
elsoc_wide_2018_2021$m02_w01_fac[elsoc_wide_2018_2021$m02_w01==-888]=NA

### ola 3 #########

elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==1]="Trabaja Jornada Completa"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==2]="Trabaja Jornada Parcial"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==3]="Estudia y trabaja"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==4]="Solo estudia"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==5]="Jubilado"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==6]="Desempleado"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==7]="Tareas no remuneradas"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==8]="Enfermedad incapacitante"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==9]="No trabaja ni busca trabajo"
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==-999]=NA
elsoc_wide_2018_2021$m02_w03_fac[elsoc_wide_2018_2021$m02_w03==-888]=NA

####### ola 5 ############

elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==1]="Trabaja Jornada Completa"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==2]="Trabaja Jornada Parcial"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==3]="Estudia y trabaja"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==4]="Solo estudia"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==5]="Jubilado"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==6]="Desempleado"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==7]="Tareas no remuneradas"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==8]="Enfermedad incapacitante"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==9]="No trabaja ni busca trabajo"
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==-999]=NA
elsoc_wide_2018_2021$m02_w05_fac[elsoc_wide_2018_2021$m02_w05==-888]=NA

###################### Percepcion de igualdad de posicion #############
# Nota: la pregunta p18_13 solo se encuentra disponible a la ola 4 y 5


############### Participacion es organizaciones sociales #################

# Nota: no se encuentra disponible para todas las olas, solo ola 1 y 3

############### Igualdad de oportunidades #####################

# Nota: no se encuentra disponible para todas las olas, solo ola 1 y 3

#### Migrantes ################

### ola 1 ####
elsoc_wide_2018_2021$cuestion_mig_w01_fac[elsoc_wide_2018_2021$cuestion_mig_w01==1]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w01_fac[elsoc_wide_2018_2021$cuestion_mig_w01==2]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w01_fac[is.na(elsoc_wide_2018_2021$cuestion_mig_w01)]="No Migrante"

#### ola 3 ########

elsoc_wide_2018_2021$cuestion_mig_w03_fac[elsoc_wide_2018_2021$cuestion_mig_w03==1]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w03_fac[elsoc_wide_2018_2021$cuestion_mig_w03==2]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w03_fac[elsoc_wide_2018_2021$cuestion_mig_w03==3]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w03_fac[is.na(elsoc_wide_2018_2021$cuestion_mig_w03)]="No Migrante"

unique(elsoc_wide_2018_2021$cuestion_mig_w05_fac)


###### ola 5 #########

elsoc_wide_2018_2021$cuestion_mig_w05_fac[elsoc_wide_2018_2021$cuestion_mig_w05==1]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w05_fac[elsoc_wide_2018_2021$cuestion_mig_w05==2]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w05_fac[elsoc_wide_2018_2021$cuestion_mig_w05==3]="Migrante"
elsoc_wide_2018_2021$cuestion_mig_w05_fac[is.na(elsoc_wide_2018_2021$cuestion_mig_w05)]="No Migrante"


########### Pueblo originario ###########
# Nota: no se encuentra disponible para las olas 1, 2 y 5

