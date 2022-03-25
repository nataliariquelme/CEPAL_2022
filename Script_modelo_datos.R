pacman::p_load(dplyr, car, summarytools, webshot, sjmisc, readxl, panelr, survey, ggplot2, summarytools, shadowtext)

# Cargar base elsoc longitudinal (2016-2019)
load("ELSOC_Wide_2016_2021_v1.00_R.RData") 

data  <- elsoc_wide_2016_2021 %>%  
  filter(tipo_atricion == 1 | tipo_atricion == 9| tipo_atricion ==17 )### escogemos a aquellas personas que responden en nuestras olas de interés.

data <- data %>% car::recode(., "c(-888,-999)=NA")

# Confianza Institucional

label(data$c05_01_w03) <- "Confianza en el gobierno (2018)."
label(data$c05_01_w04) <- "Confianza en el gobierno (2019)."
label(data$c05_01_w05) <- "Confianza en el gobierno (2020)."
data %>% select (c05_01_w03, c05_01_w04, c05_01_w05)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

label(data$c05_02_w03) <- "Confianza en los partidos politicos (2018)."
label(data$c05_02_w04) <- "Confianza en los partidos politicos (2019)."
label(data$c05_02_w05) <- "Confianza en los partidos politicos (2020)."
data %>% select (c05_02_w03, c05_02_w04, c05_02_w05)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

label(data$c05_08_w03) <- "Confianza en el presidente/a de la republica (2018)."
label(data$c05_08_w04) <- "Confianza en el presidente/a de la republica (2019)."
label(data$c05_08_w05) <- "Confianza en el presidente/a de la republica (2020)."
data %>% select (c05_08_w03, c05_08_w04, c05_08_w05)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

# Percepcion de desigualdad
data$per_desigualdad_w03[data$c18_11_w03==1]<- "En desacuerdo"
data$per_desigualdad_w03[data$c18_11_w03==2]<- "En desacuerdo"
data$per_desigualdad_w03[data$c18_11_w03==3]<- "Ni de acuerdo ni en desacuerdo"
data$per_desigualdad_w03[data$c18_11_w03==4]<- "De acuerdo"
data$per_desigualdad_w03[data$c18_11_w03==5]<- "De acuerdo"

data$per_desigualdad_w04[data$c18_11_w04==1]<- "En desacuerdo"
data$per_desigualdad_w04[data$c18_11_w04==2]<- "En desacuerdo"
data$per_desigualdad_w04[data$c18_11_w04==3]<- "Ni de acuerdo ni en desacuerdo"
data$per_desigualdad_w04[data$c18_11_w04==4]<- "De acuerdo"
data$per_desigualdad_w04[data$c18_11_w04==5]<- "De acuerdo"

data$per_desigualdad_w05[data$c18_11_w05==1]<- "En desacuerdo"
data$per_desigualdad_w05[data$c18_11_w05==2]<- "En desacuerdo"
data$per_desigualdad_w05[data$c18_11_w05==3]<- "Ni de acuerdo ni en desacuerdo"
data$per_desigualdad_w05[data$c18_11_w05==4]<- "De acuerdo"
data$per_desigualdad_w05[data$c18_11_w05==5]<- "De acuerdo"

data %>% select (per_desigualdad_w03, per_desigualdad_w04, per_desigualdad_w05)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

# Percepcion de movilidad social

data$movilidad_social_wo3[data$d05_04_w03==1]<- "Poco importante"
data$movilidad_social_wo3[data$d05_04_w03==2]<- "Poco importante"
data$movilidad_social_wo3[data$d05_04_w03==3]<- "Algo importante"
data$movilidad_social_wo3[data$d05_04_w03==4]<- "Muy importante"
data$movilidad_social_wo3[data$d05_04_w03==5]<- "Muy importante"

data$movilidad_social_wo4[data$d05_04_w04==1]<- "Poco importante"
data$movilidad_social_wo4[data$d05_04_w04==2]<- "Poco importante"
data$movilidad_social_wo4[data$d05_04_w04==3]<- "Algo importante"
data$movilidad_social_wo4[data$d05_04_w04==4]<- "Muy importante"
data$movilidad_social_wo4[data$d05_04_w04==5]<- "Muy importante"

data$movilidad_social_wo5[data$d05_04_w05==1]<- "Poco importante"
data$movilidad_social_wo5[data$d05_04_w05==2]<- "Poco importante"
data$movilidad_social_wo5[data$d05_04_w05==3]<- "Algo importante"
data$movilidad_social_wo5[data$d05_04_w05==4]<- "Muy importante"
data$movilidad_social_wo5[data$d05_04_w05==5]<- "Muy importante"

data %>% select (movilidad_social_wo3, movilidad_social_wo4, movilidad_social_wo5)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

# Sexo 
label(data$m0_sexo_w03) <- "Sexo del entrevistado (2018)."
label(data$m0_sexo_w04) <- "Sexo del entrevistado (2019)."
label(data$m0_sexo_w05) <- "Sexo del entrevistado (2020)"
data %>% select (m0_sexo_w03, m0_sexo_w04, m0_sexo_w05)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

## Edad

### OLA 3 ##############

data$m0_edad_w03_fac <- as.character(car::recode(data$m0_edad_w03, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))

data$m0_edad_w03_fac[data$m0_edad_w03_fac==1]="18 a 30 años"
data$m0_edad_w03_fac[data$m0_edad_w03_fac==2]="31 a 50 años"
data$m0_edad_w03_fac[data$m0_edad_w03_fac==3]="51 a 70 años"
data$m0_edad_w03_fac[data$m0_edad_w03_fac==4]="71 o más años"

##### OLA 4 ##########

data$m0_edad_w04_fac <- as.character(car::recode(data$m0_edad_w04, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


data$m0_edad_w04_fac[data$m0_edad_w04_fac==1]="18 a 30 años"
data$m0_edad_w04_fac[data$m0_edad_w04_fac==2]="31 a 50 años"
data$m0_edad_w04_fac[data$m0_edad_w04_fac==3]="51 a 70 años"
data$m0_edad_w04_fac[data$m0_edad_w04_fac==4]="71 o más años"

####### OLA 5 ######################

data$m0_edad_w05_fac <- as.character(car::recode(data$m0_edad_w05, recodes = "1:30 = 1; 31:50 = 2; 51:70 = 3; 71:100 = 4"))


data$m0_edad_w05_fac[data$m0_edad_w05_fac==1]="18 a 30 años"
data$m0_edad_w05_fac[data$m0_edad_w05_fac==2]="31 a 50 años"
data$m0_edad_w05_fac[data$m0_edad_w05_fac==3]="51 a 70 años"
data$m0_edad_w05_fac[data$m0_edad_w05_fac==4]="71 o más años"

data %>% select (m0_edad_w03_fac, m0_edad_w04_fac, m0_edad_w05_fac)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

### Educacion

data$m01_w03_fac[data$m01_w03==1]="Menos que media completa"
data$m01_w03_fac[data$m01_w03==2]="Menos que media completa"
data$m01_w03_fac[data$m01_w03==3]="Menos que media completa"
data$m01_w03_fac[data$m01_w03==4]="Menos que media completa"
data$m01_w03_fac[data$m01_w03==5]="Media completa"
data$m01_w03_fac[data$m01_w03==6]="Educacion tecnica superior"
data$m01_w03_fac[data$m01_w03==7]="Educacion tecnica superior"
data$m01_w03_fac[data$m01_w03==8]="Educacion universitaria y Postgrado"
data$m01_w03_fac[data$m01_w03==9]="Educacion universitaria y Postgrado"
data$m01_w03_fac[data$m01_w03==10]="Educacion universitaria y Postgrado"
data$m01_w03_fac[data$m01_w03==-999]=NA

data$m01_w04_fac[data$m01_w04==1]="Menos que media completa"
data$m01_w04_fac[data$m01_w04==2]="Menos que media completa"
data$m01_w04_fac[data$m01_w04==3]="Menos que media completa"
data$m01_w04_fac[data$m01_w04==4]="Menos que media completa"
data$m01_w04_fac[data$m01_w04==5]="Media completa"
data$m01_w04_fac[data$m01_w04==6]="Educacion tecnica superior"
data$m01_w04_fac[data$m01_w04==7]="Educacion tecnica superior"
data$m01_w04_fac[data$m01_w04==8]="Educacion universitaria y Postgrado"
data$m01_w04_fac[data$m01_w04==9]="Educacion universitaria y Postgrado"
data$m01_w04_fac[data$m01_w04==10]="Educacion universitaria y Postgrado"
data$m01_w04_fac[data$m01_w04==-999]=NA

data$m01_w05_fac[data$m01_w05==1]="Menos que media completa"
data$m01_w05_fac[data$m01_w05==2]="Menos que media completa"
data$m01_w05_fac[data$m01_w05==3]="Menos que media completa"
data$m01_w05_fac[data$m01_w05==4]="Menos que media completa"
data$m01_w05_fac[data$m01_w05==5]="Media completa"
data$m01_w05_fac[data$m01_w05==6]="Educacion tecnica superior"
data$m01_w05_fac[data$m01_w05==7]="Educacion tecnica superior"
data$m01_w05_fac[data$m01_w05==8]="Educacion universitaria y Postgrado"
data$m01_w05_fac[data$m01_w05==9]="Educacion universitaria y Postgrado"
data$m01_w05_fac[data$m01_w05==10]="Educacion universitaria y Postgrado"
data$m01_w05_fac[data$m01_w05==-999]=NA

label(data$m01_w03_fac) <- "Nivel educacional (2018)"
label(data$m01_w04_fac) <- "Nivel educacional (2019)"
label(data$m01_w05_fac) <- "Nivel educacional (2020)"

data %>% select (m01_w03_fac, m01_w04_fac, m01_w05_fac)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

## Ingresos

### ola 3 ##########

data$m13_w03_fac <- as.character(car::recode(data$m13_w03, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


data$m13_w03_fac[data$m13_w03_fac==1]="Menos de 250.000"
data$m13_w03_fac[data$m13_w03_fac==2]="Entre 250.000 a 350.000"
data$m13_w03_fac[data$m13_w03_fac==3]="Entre 350.000 a 450.000"
data$m13_w03_fac[data$m13_w03_fac==4]="Entre 450.000 a 700.000"
data$m13_w03_fac[data$m13_w03_fac==5]="Mas de 700.000"
data$m13_w03_fac[data$m13_w03_fac==-999]=NA
data$m13_w03_fac[data$m13_w03_fac==-888]=NA


#### ola 4 #############
data$m13_w04_fac <- as.character(car::recode(data$m13_w04, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


data$m13_w04_fac[data$m13_w04_fac==1]="Menos de 250.000"
data$m13_w04_fac[data$m13_w04_fac==2]="Entre 250.000 a 350.000"
data$m13_w04_fac[data$m13_w04_fac==3]="Entre 350.000 a 450.000"
data$m13_w04_fac[data$m13_w04_fac==4]="Entre 450.000 a 700.000"
data$m13_w04_fac[data$m13_w04_fac==5]="Mas de 700.000"
data$m13_w04_fac[data$m13_w04_fac==-999]=NA
data$m13_w04_fac[data$m13_w04_fac==-888]=NA

####### ola 5 ##########

data$m13_w05_fac <- as.character(car::recode(data$m13_w05, recodes = "0:249999 = 1; 250000:349999 = 2; 350000:449999 = 3; 450000:699999 = 4; 700000:15000000 = 5"))


data$m13_w05_fac[data$m13_w05_fac==1]="Menos de 250.000"
data$m13_w05_fac[data$m13_w05_fac==2]="Entre 250.000 a 350.000"
data$m13_w05_fac[data$m13_w05_fac==3]="Entre 350.000 a 450.000"
data$m13_w05_fac[data$m13_w05_fac==4]="Entre 450.000 a 700.000"
data$m13_w05_fac[data$m13_w05_fac==5]="Mas de 700.000"
data$m13_w05_fac[data$m13_w05_fac==-999]=NA
data$m13_w05_fac[data$m13_w05_fac==-888]=NA

data %>% select (m13_w03_fac, m13_w04_fac, m13_w05_fac)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

## MIgrante

#### Migrantes ################

### ola 1 ####
data$cuestion_mig_w03_fac[data$cuestion_mig_w03==1]="Migrante"
data$cuestion_mig_w03_fac[data$cuestion_mig_w03==2]="Migrante"
data$cuestion_mig_w03_fac[is.na(data$cuestion_mig_w03)]="No Migrante"

#### ola 3 ########

data$cuestion_mig_w04_fac[data$cuestion_mig_w04==1]="Migrante"
data$cuestion_mig_w04_fac[data$cuestion_mig_w04==2]="Migrante"
data$cuestion_mig_w04_fac[data$cuestion_mig_w04==3]="Migrante"
data$cuestion_mig_w04_fac[is.na(data$cuestion_mig_w04)]="No Migrante"

unique(data$cuestion_mig_w05_fac)


###### ola 5 #########

data$cuestion_mig_w05_fac[data$cuestion_mig_w05==1]="Migrante"
data$cuestion_mig_w05_fac[data$cuestion_mig_w05==2]="Migrante"
data$cuestion_mig_w05_fac[data$cuestion_mig_w05==3]="Migrante"
data$cuestion_mig_w05_fac[is.na(data$cuestion_mig_w05)]="No Migrante"


data %>% select (cuestion_mig_w03_fac, cuestion_mig_w04_fac, cuestion_mig_w05_fac)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

## Participacion en mercado laboral

data$m02_w03_fac[data$m02_w03==1]= "Participa"
data$m02_w03_fac[data$m02_w03==2]= "Participa"
data$m02_w03_fac[data$m02_w03==3]= "Participa"
data$m02_w03_fac[data$m02_w03==4]= "No Participa"
data$m02_w03_fac[data$m02_w03==5]= "No Participa"
data$m02_w03_fac[data$m02_w03==6]= "No Participa"
data$m02_w03_fac[data$m02_w03==7]= "No Participa"
data$m02_w03_fac[data$m02_w03==8]= "No Participa"
data$m02_w03_fac[data$m02_w03==9]= "No Participa"

data$m02_w04_fac[data$m02_w04==1]= "Participa"
data$m02_w04_fac[data$m02_w04==2]= "Participa"
data$m02_w04_fac[data$m02_w04==3]= "Participa"
data$m02_w04_fac[data$m02_w04==4]= "No Participa"
data$m02_w04_fac[data$m02_w04==5]= "No Participa"
data$m02_w04_fac[data$m02_w04==6]= "No Participa"
data$m02_w04_fac[data$m02_w04==7]= "No Participa"
data$m02_w04_fac[data$m02_w04==8]= "No Participa"
data$m02_w04_fac[data$m02_w04==9]= "No Participa"

data$m02_w05_fac[data$m02_w05==1]= "Participa"
data$m02_w05_fac[data$m02_w05==2]= "Participa"
data$m02_w05_fac[data$m02_w05==3]= "Participa"
data$m02_w05_fac[data$m02_w05==4]= "No Participa"
data$m02_w05_fac[data$m02_w05==5]= "No Participa"
data$m02_w05_fac[data$m02_w05==6]= "No Participa"
data$m02_w05_fac[data$m02_w05==7]= "No Participa"
data$m02_w05_fac[data$m02_w05==8]= "No Participa"
data$m02_w05_fac[data$m02_w05==9]= "No Participa"

data %>% select (m02_w03_fac, m02_w04_fac, m02_w05_fac)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

# Guardamos base

save(data,file = "data.RData")
# Cargar base elsoc longitudinal (2016-2019) creada en "preparacion"
load("data.RData")

## Confianza institucional 

data <- data %>%
  rowwise() %>%
  mutate(conf_institucional_w03 =
           mean(c(c05_01_w03, c05_08_w03, c05_02_w03), na.rm = T))
data <- data %>%
  rowwise() %>%
  mutate(conf_institucional_w04 =
           mean(c(c05_01_w04, c05_08_w04, c05_02_w04), na.rm = T))
data <- data %>%
  rowwise() %>%
  mutate(conf_institucional_w05 =
           mean(c(c05_01_w05, c05_08_w05, c05_02_w05), na.rm = T))

data %>% select( conf_institucional_w03, conf_institucional_w04, conf_institucional_w05)  %>%  dfSummary(, graph.col = FALSE)

### Percepcion de desigualdad##

data %>% select (per_desigualdad_w03, per_desigualdad_w04, per_desigualdad_w05)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

## Percepcion de movilidad social ###

data %>% select (movilidad_social_wo3, movilidad_social_wo4, movilidad_social_wo5)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

## Expectativas de movilidad social ###

data$exp_mov_w03 <- data$d01_03_w03-data$d01_01_w03
data$exp_mov_w04 <- data$d01_03_w04-data$d01_01_w04
data$exp_mov_w05 <- data$d01_03_w05-data$d01_01_w05

data$exp_mov_fac_w03[data$exp_mov_w03<0]<- "Expectativas negativas"
data$exp_mov_fac_w04[data$exp_mov_w04<0]<- "Expectativas negativas"
data$exp_mov_fac_w05[data$exp_mov_w05<0]<- "Expectativas negativas"

data$exp_mov_fac_w03[data$exp_mov_w03==0]<- "Neutro"
data$exp_mov_fac_w04[data$exp_mov_w04==0]<- "Neutro"
data$exp_mov_fac_w05[data$exp_mov_w05==0]<- "Neutro"

data$exp_mov_fac_w03[data$exp_mov_w03>0]<- "Expectativas positivas"
data$exp_mov_fac_w04[data$exp_mov_w04>0]<- "Expectativas positivas"
data$exp_mov_fac_w05[data$exp_mov_w05>0]<- "Expectativas positivas"

data %>% select( exp_mov_fac_w03, exp_mov_fac_w04, exp_mov_fac_w05)  %>%  dfSummary(, graph.col = FALSE)

## Confianza Institucional ###

data$conf_institucional_w03 <- ifelse((data$conf_institucional_w03<1.5), 1,
                                      ifelse((data$conf_institucional_w03<2.5), 2,
                                             ifelse((data$conf_institucional_w03<3.5), 3,
                                                    ifelse((data$conf_institucional_w03<4.5), 4,
                                                           ifelse((data$conf_institucional_w03<=5), 5, NA)))))
data$conf_institucional_w04 <- ifelse((data$conf_institucional_w04<1.5), 1,
                                      ifelse((data$conf_institucional_w04<2.5), 2,
                                             ifelse((data$conf_institucional_w04<3.5), 3,
                                                    ifelse((data$conf_institucional_w04<4.5), 4,
                                                           ifelse((data$conf_institucional_w04<=5), 5, NA)))))
data$conf_institucional_w05 <- ifelse((data$conf_institucional_w05<1.5), 1,
                                      ifelse((data$conf_institucional_w05<2.5), 2,
                                             ifelse((data$conf_institucional_w05<3.5), 3,
                                                    ifelse((data$conf_institucional_w05<4.5), 4,
                                                           ifelse((data$conf_institucional_w05<=5), 5, NA)))))

data$conf_institucional_w03 <- car::recode(data$conf_institucional_w03, "c(1,2)=1; 3=2; c(4,5)=3")
data$conf_institucional_w04 <- car::recode(data$conf_institucional_w04, "c(1,2)=1; 3=2; c(4,5)=3")
data$conf_institucional_w05 <- car::recode(data$conf_institucional_w05, "c(1,2)=1; 3=2; c(4,5)=3")

data$conf_institucional_w03<- factor(data$conf_institucional_w03, levels = c(1, 2, 3), labels = c("Nada o Poca", "Algo", "Bastante o Mucha"))   
data$conf_institucional_w04<- factor(data$conf_institucional_w04, levels = c(1, 2, 3), labels = c("Nada o Poca", "Algo", "Bastante o Mucha"))   
data$conf_institucional_w05<- factor(data$conf_institucional_w05, levels = c(1, 2, 3), labels = c("Nada o Poca", "Algo", "Bastante o Mucha")) 

data %>% select( conf_institucional_w03, conf_institucional_w04, conf_institucional_w05)  %>% sjlabelled::as_label(.)  %>%  dfSummary(, graph.col = FALSE)

## Visualizacion de datos
elsoc_long <- long_panel(data = data, #base de datos formato wide
                         prefix = "_w0", #caracteres antes de la etiqueta de cada ola
                         begin = 3, #etiqueta de la primera ola
                         end = 5, #etiqueta de la última ola
                         label_location = "end", #indica donde se localiza la etiqueta asociada a la ola 
                         id = "idencuesta", #indica identificador individual
                         wave = "ola") #nombre que tomará la variable que indica periodo. 

#Recode variable "ola" correspondiente a la ola de medición.
elsoc_long$ola <- factor(elsoc_long$ola,labels = c('2018', '2019', '2020'))
elsoc_long$ola <- sjlabelled::set_label(elsoc_long$ola, label = c("Ola de Medición"))
#etiquetamos variable

elsoc_diseno <- svydesign(ids = ~segmento, #muestreo por conglomerado a nivel de manzanas (segmento)
                          strata = ~estrato, #muestreo estratificado a nivel ciudad (estato)
                          weights = ~ponderador02, #ponderador de corte transversal
                          nest = TRUE,
                          data = elsoc_long)

## Confianza institucional

#Paso 1
datos.conf.institucional <- data.frame((svytable(~conf_institucional + ola + idencuesta, elsoc_diseno, round = F))) %>% dplyr::filter(Freq>0)  %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit()
#Paso 2
etiquetas.conf.institucional <- data.frame((svytable(~conf_institucional + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit() %>% 
  mutate(idencuesta = 1)

alluvial_conf.institucional <- ggplot(datos.conf.institucional, aes(x = ola, fill = conf_institucional, stratum = conf_institucional,
                                                                    alluvium = idencuesta, y = porcentaje))+
  ggalluvial::geom_flow(alpha = .66) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  ylab(label = NULL) +
  xlab(label = NULL) + 
  theme(legend.position = 'top',
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  scale_fill_viridis_d(begin = 0, end = .95, direction = -1, option = 'viridis') +
  geom_shadowtext(data = etiquetas.conf.institucional, 
                  aes(label = ifelse(porcentaje > 0.03 , scales::percent(porcentaje, accuracy = .1),"")),
                  position = position_stack(vjust = .5),
                  show.legend = FALSE,
                  size = 4,
                  color = rep('white'),
                  bg.colour='grey30')

alluvial_conf.institucional

## Percepción de desigualdad

#Paso 1
datos.per.desigualdad <- data.frame((svytable(~per_desigualdad + ola + idencuesta, elsoc_diseno, round = F))) %>% dplyr::filter(Freq>0)  %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit()
#Paso 2
etiquetas.per.desigualdad <- data.frame((svytable(~per_desigualdad + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit() %>% 
  mutate(idencuesta = 1)

alluvial_per.desigualdad <- ggplot(datos.per.desigualdad, aes(x = ola, fill = per_desigualdad, stratum = per_desigualdad,
                                                              alluvium = idencuesta, y = porcentaje))+
  ggalluvial::geom_flow(alpha = .66) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  ylab(label = NULL) +
  xlab(label = NULL) + 
  theme(legend.position = 'top',
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  scale_fill_viridis_d(begin = 0, end = .95, direction = -1, option = 'viridis') +
  geom_shadowtext(data = etiquetas.per.desigualdad , 
                  aes(label = ifelse(porcentaje > 0.03 , scales::percent(porcentaje, accuracy = .1),"")),
                  position = position_stack(vjust = .5),
                  show.legend = FALSE,
                  size = 4,
                  color = rep('white'),
                  bg.colour='grey30')

alluvial_per.desigualdad

## Expectativas de movilidad social

#Paso 1
datos.exp.mov <- data.frame((svytable(~exp_mov_fac + ola + idencuesta, elsoc_diseno, round = F))) %>% dplyr::filter(Freq>0)  %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit()
#Paso 2
etiquetas.exp.mov <- data.frame((svytable(~exp_mov_fac + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit() %>% 
  mutate(idencuesta = 1)

alluvial_exp.mov <- ggplot(datos.exp.mov, aes(x = ola, fill = exp_mov_fac, stratum = exp_mov_fac,
                                              alluvium = idencuesta, y = porcentaje))+
  ggalluvial::geom_flow(alpha = .66) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  ylab(label = NULL) +
  xlab(label = NULL) + 
  theme(legend.position = 'top',
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  scale_fill_viridis_d(begin = 0, end = .95, direction = -1, option = 'viridis') +
  geom_shadowtext(data = etiquetas.exp.mov , 
                  aes(label = ifelse(porcentaje > 0.03 , scales::percent(porcentaje, accuracy = .1),"")),
                  position = position_stack(vjust = .5),
                  show.legend = FALSE,
                  size = 4,
                  color = rep('white'),
                  bg.colour='grey30')

alluvial_exp.mov



### Creación de la base de datos de modelo 

#### Confianza en las instituciones

data_conf_w03 <- data %>% 
  select(conf_institucional_w03, m0_sexo_w03, m0_edad_w03_fac, m01_w03_fac, 
         m13_w03_fac, m02_w03_fac, ponderador02_w03)

data_conf_w04 <- data %>% 
  select(conf_institucional_w04,  m0_sexo_w04, m0_edad_w04_fac, m01_w04_fac,
         m13_w04_fac, m02_w04_fac,ponderador02_w04)

data_conf_w05 <- data %>% 
  select(conf_institucional_w05, m0_sexo_w05, m0_edad_w05_fac, 
         m01_w05_fac, m13_w05_fac, m02_w05_fac, ponderador02_w05)

#### percepción de la desigualdad

data_per_des_w03 <- data %>% 
  select(per_desigualdad_w03, m0_sexo_w03, m0_edad_w03_fac, m01_w03_fac, 
         m13_w03_fac, m02_w03_fac, ponderador02_w03)

data_per_des_w04 <- data %>% 
  select(per_desigualdad_w04,  m0_sexo_w04, m0_edad_w04_fac, m01_w04_fac, 
         m13_w04_fac, m02_w04_fac, ponderador02_w04)

data_per_des_w05 <- data %>% 
  select(per_desigualdad_w05, m0_sexo_w05, m0_edad_w05_fac, m01_w05_fac, 
         m13_w05_fac, m02_w05_fac, ponderador02_w05)

#### Percepción de movilidad social

data_per_mov_w03 <- data %>% 
  select(movilidad_social_wo3, m0_sexo_w03, m0_edad_w03_fac, m01_w03_fac,
         m13_w03_fac, m02_w03_fac, ponderador02_w03)

data_per_mov_w04 <- data %>% 
  select(movilidad_social_wo4,  m0_sexo_w04, m0_edad_w04_fac, m01_w04_fac, 
         m13_w04_fac, m02_w04_fac, ponderador02_w04)

data_per_mov_w05 <- data %>% 
  select(movilidad_social_wo5, m0_sexo_w05, m0_edad_w05_fac, m01_w05_fac,
         m13_w05_fac, m02_w05_fac, ponderador02_w05)

#### Expectativas de movilidad social

data_exp_mov_w03 <- data %>% 
  select(exp_mov_fac_w03, m0_sexo_w03, m0_edad_w03_fac, m01_w03_fac, 
         m13_w03_fac, m02_w03_fac, ponderador02_w03)

data_exp_mov_w04 <- data %>% 
  select(exp_mov_fac_w04,  m0_sexo_w04, m0_edad_w04_fac, m01_w04_fac,
         m13_w04_fac, m02_w04_fac, ponderador02_w04)

data_exp_mov_w05 <- data %>% 
  select(exp_mov_fac_w05, m0_sexo_w05, m0_edad_w05_fac, m01_w05_fac, 
         m13_w05_fac, m02_w05_fac, ponderador02_w05)

## Seleccion modelo

#### Confianza institucional w03

mod_nulo <- lm(conf_institucional_w03 ~ 1,  weights = ponderador02_w03, data_conf_w03)
mod_full <-  lm(conf_institucional_w03~., weights = ponderador02_w03, data = data_conf_w03)


modelo <-  step(mod_nulo,
                scope=list(lower=formula(mod_nulo),
                           upper=formula(mod_full)),
                direction="both")

### modelo seleccionado
summary(modelo)

#### Confianza institucional w04

mod_nulo <- lm(conf_institucional_w04~1,  weights = ponderador_02_w04, data_conf_w03)
mod_full <-  lm(conf_institucional_w04~., weights = ponderador_02_w04, data = data_conf_w03)


modelo <-  step(mod_nulo,
                scope=list(lower=formula(mod_nulo),
                           upper=formula(mod_full)),
                direction="both")

### modelo seleccionado
summary(modelo)

#### Confianza institucional w05

mod_nulo <- lm(conf_institucional_w05~1,weights = ponderador_02_w05, data_conf_w05)
mod_full <-  lm(conf_institucional_w05~.,weights = ponderador_02_w05, data = data_conf_w05)


modelo <-  step(mod_nulo,
                scope=list(lower=formula(mod_nulo),
                           upper=formula(mod_full)),
                direction="both")

### modelo seleccionado
summary(modelo)

