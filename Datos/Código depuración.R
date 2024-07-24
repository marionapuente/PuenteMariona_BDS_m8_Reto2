# Paquetes

library(dplyr)

# Leer base de datos

df <- read.csv('Base de datos original.csv')

# Eliminar las variables que no vamos a utilizar

df <- select(df, -"idno", -"name", -"essround", -"edition", -"proddate", -"dweight", -"pweight", -"prob", -"stratum", -"psu", -"freehms", -"lrnobed", -"impcntr", -"wrclmch")

# Recodificar los valores de las varirables cualitativas

country_mapping <- c(
  AT = "Áustria", CH = "Suiza", DE = "Alemania", FI = "Finlandia", GB = "Reino Unido",
  HR = "Croacia", HU = "Hungría", LT = "Lituania", NL = "Países Bajos", NO = "Noruega",
  IE = "Irlanda", SI = "Eslovenia", SK = "Eslovaquia"
)
df <- df %>%
  mutate(cntry = recode(cntry, !!!country_mapping))
df$cntry <- factor(df$cntry)

gender_mapping <- c(
  `1` = "Hombre",
  `2` = "Mujer",
  `9` = "No answer"
)
df <- df %>%
  mutate(gndr = recode(gndr, !!!gender_mapping))
df$gndr <- factor(df$gndr)

# Identificar los valores perdidos

df[df == 'No answer'] <- NA
df[df == 999] <- NA
df[df == 77] <- NA
df[df == 88] <- NA
df[df == 99] <- NA

# Recodificar variable edad en cualitativa

df$age_category <- cut(
  df$agea,
  breaks = c(-Inf, 29, 44, 59, 74, Inf),
  labels = c("menor de 30", "entre 30 y 44", "entre 45 y 59", "entre 60 y 74", "75 o mayor"),
  right = TRUE
)
df$age_category <- factor(df$age_category)

df <- select(df, -"agea")

# Recodificar nombres de las variables

names(df) <- c("País", "Apego emocional al propio país", "Apego emocional a Europa", "Género", "Rango de edad")

# Guardar base de datos depurada

write.csv(df, 'Base de datos depurada.csv')