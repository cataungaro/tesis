library(readxl)
library(tidyverse)

dccp <- read_excel("data/base_dccp_para_analisis.xls")
View(dccp)

names(dccp)

dcpp_incluidos <- dccp %>% filter(paciente_incluido == 1)

# Proporción de NAs en variable de resultado
prop_na_dcpp <- mean(is.na(dcpp_incluidos$dccp))

dcpp_not_na <- filter(dcpp_incluidos, dccp != "NA")

# Calcular la incidencia de DCPP (de 67 pacientes incluídos, se pudo calcular la variable de resultado en 47)
incidencia_dcpp <- mean(dcpp_not_na$dccp)

dif_na_para_icc <- dcpp_incluidos %>% select(dif_na)

  
