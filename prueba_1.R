library(tidyverse)
library(summarytools)
library(questionr)

data <- elsoc_wide_2016_2021 %>% 
  filter(tipo_atricion == 1 | tipo_atricion == 9| tipo_atricion ==17 )

attach(data)

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
           wtd.mean(c(c05_01_w05, c05_08_w05, c05_02_w05),ponderador02_w05))

data %>% select( conf_institucional_w03, conf_institucional_w04, 
                conf_institucional_w05)


multi.table(data[,c(c05_01_w05, c05_08_w05, c05_02_w05)], true.codes=list("Y"), weights=data$ponderador02_w05)
