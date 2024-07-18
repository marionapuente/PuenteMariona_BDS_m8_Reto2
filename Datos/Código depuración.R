# Paquetes

library(dplyr)

# Leer base de datos

df <- read.csv('Base de datos original.csv')

# Eliminar las variables que no vamos a utilizar

df <- select(df, -"name", -"essround", -"edition", -"proddate", -"dweight", -"pweight", -"prob", -"stratum", -"psu")

# Recodificar los valores de las varirables cualitativas

country_mapping <- c(
  AL = "Albania", AT = "Austria", BE = "Belgium", BG = "Bulgaria", CH = "Switzerland",
  CY = "Cyprus", CZ = "Czechia", DE = "Germany", DK = "Denmark", EE = "Estonia",
  ES = "Spain", FI = "Finland", FR = "France", GB = "United Kingdom", GE = "Georgia",
  GR = "Greece", HR = "Croatia", HU = "Hungary", IE = "Ireland", IS = "Iceland",
  IL = "Israel", IT = "Italy", LT = "Lithuania", LU = "Luxembourg", LV = "Latvia",
  ME = "Montenegro", MK = "North Macedonia", NL = "Netherlands", NO = "Norway",
  PL = "Poland", PT = "Portugal", RO = "Romania", RS = "Serbia", RU = "Russian Federation",
  SE = "Sweden", SI = "Slovenia", SK = "Slovakia", TR = "Turkey", UA = "Ukraine",
  XK = "Kosovo"
)
df <- df %>%
  mutate(cntry = recode(cntry, !!!country_mapping))
df$cntry <- factor(df$cntry)

gender_mapping <- c(
  `1` = "Male",
  `2` = "Female",
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
df$freehms[df$freehms %in% c(7, 8, 9)] <- NA
df$lrnobed[df$lrnobed %in% c(7, 8, 9)] <- NA
df$impcntr[df$impcntr %in% c(7, 8, 9)] <- NA
df$wrclmch[df$wrclmch %in% c(6, 7, 8, 9)] <- NA

# Recodificar variable edad en cualitativa

df$age_category <- cut(
  df$agea,
  breaks = c(-Inf, 29, 44, 59, 74, Inf),
  labels = c("menor de 30", "entre 30 y 44", "entre 45 y 59", "entre 60 y 74", "75 o mayor"),
  right = TRUE
)
df$age_category <- factor(df$age_category)

# Guardar base de datos depurada

write.csv(df, 'Base de datos depurada.csv')






#de moment no seguim

ggplot(data.frame(df$atchctr), aes(x=df$atchctr)) +
  geom_density(fill="red", alpha=0.5) +
  labs(title="Gráfico de Densidad de la variable", x="Valores", y="Densidad")

#aquest es el q magradaaa
df_prop <- df %>%
  filter(!is.na(atchctr)) %>%
  group_by(atchctr) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))
ggplot(df_prop, aes(x=atchctr, y=prop)) +
  geom_smooth(se=FALSE, method="loess", color="red", size=1) +
  labs(title="Distribución de la variable en escala Likert con suavización", x="Valores", y="Proporción") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme_minimal()


print(summary(df[, !names(df) %in% "idno"]))
print(table(df$cntry))

library(ggplot2)
numeric_vars <- c("agea", "freehms", "lrnobed", "impcntr", "atchctr", "atcherp", "wrclmch")
histograms <- lapply(numeric_vars, function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(binwidth = 1, fill = "darkorange", color = "black") +
    labs(title = var, x = var, y = "Frequency")
})
print(histograms[[1]])
print(histograms[[2]])
print(histograms[[3]])
print(histograms[[4]])
print(histograms[[5]])
print(histograms[[6]])
print(histograms[[7]])

boxplots <- lapply(numeric_vars, function(var) {
  ggplot(df, aes_string(y = var)) +
    geom_boxplot(fill = "darkred", color = "black") +
    labs(title = var, y = var)
})
print(boxplots[[7]])

ggplot(df, aes(x = gndr)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribución de Género", x = "Género", y = "Frecuencia") +
  theme_minimal()
ggplot(df, aes(x = cntry)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribución de Países", x = "País", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

datos <- c(2354, 1563, 1562, 2420, 2118, 2017, 1365, 1695, 1337, 1442, 1248, 1384, 1684)
datos <- data.frame(datos)
names(datos) <- c("Observaciones")
ggplot(datos, aes(x = Observaciones)) +
  geom_histogram(binwidth = 200, fill = "peru", color = "black") +
  labs(title = "Histograma",
       x = "Número de observaciones por país", y = "Frecuencia") +
  theme_minimal()
summary(datos)
print(sd(datos$Observaciones))

library(corrplot)
library(psych)
df_subset <- df[, c("freehms", "lrnobed", "impcntr", "wrclmch")]
df_subset <- na.omit(df_subset)
corr_matrix <- cor(df_subset)
print(corr_matrix)
corrplot(corr_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black",
         tl.srt = 45, diag = FALSE)

df_subset2 <- df[, c("atchctr", "atcherp")]
correlacion <- cor(df_subset2$atchctr, df_subset2$atcherp, use = "complete.obs")
print(correlacion)
ggplot(df_subset2, aes(x = atchctr, y = atcherp)) +
  geom_bin2d(bins = 20) +
  scale_fill_viridis_c() +
  labs(x = "Attached country", y = "Attached Europe", title = "Heatmap de Distribución de Variables") +
  theme_minimal()

df_subset2 <- df[, c("atchctr", "atcherp", "gndr")]
df_subset2male <- subset(df_subset2, gndr == "Male")
df_subset2female <- subset(df_subset2, gndr == "Female")

ggplot(df_subset2male, aes(x = atchctr, y = atcherp)) +
  geom_bin2d(bins = 20) +
  scale_fill_viridis_c() +
  labs(x = "Attached country", y = "Attached Europe", title = "Heatmap Hombres") +
  theme_minimal()

df_subset3 <- df[, c("atchctr", "atcherp", "cntry")]
df_subset3Austria <- subset(df_subset3, cntry == "Austria")
df_subset3Croatia <- subset(df_subset3, cntry == "Croatia")
df_subset3Finland <- subset(df_subset3, cntry == "Finland")
df_subset3Germany <- subset(df_subset3, cntry == "Germany")
df_subset3Hungary <- subset(df_subset3, cntry == "Hungary")
df_subset3Ireland <- subset(df_subset3, cntry == "Ireland")
df_subset3Lithuania <- subset(df_subset3, cntry == "Lithuania")
df_subset3Netherlands <- subset(df_subset3, cntry == "Netherlands")
df_subset3Norway <- subset(df_subset3, cntry == "Norway")
df_subset3Slovakia <- subset(df_subset3, cntry == "Slovakia")
df_subset3Slovenia <- subset(df_subset3, cntry == "Slovenia")
df_subset3Switzerland <- subset(df_subset3, cntry == "Switzerland")
df_subset3UK <- subset(df_subset3, cntry == "United Kingdom")

ggplot(df_subset3Hungary, aes(x = atchctr, y = atcherp)) +
  geom_bin2d(bins = 20) +
  scale_fill_viridis_c() +
  labs(x = "Attached country", y = "Attached Europe", title = "Heatmap Hungary") +
  theme_minimal()

df$age_category <- cut(
  df$agea,
  breaks = c(-Inf, 29, 44, 59, 74, Inf),
  labels = c("menor de 30", "entre 30 y 44", "entre 45 y 59", "entre 60 y 74", "75 o mayor"),
  right = TRUE
)
ggplot(df, aes(x = age_category)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribución de Edades", x = "Edad", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
summary(df)

df_subset4 <- df[, c("atchctr", "atcherp", "age_category")]
df_subset41 <- subset(df_subset4, age_category == "menor de 30")
df_subset42 <- subset(df_subset4, age_category == "entre 30 y 44")
df_subset43 <- subset(df_subset4, age_category == "entre 45 y 59")
df_subset44 <- subset(df_subset4, age_category == "entre 60 y 74")
df_subset45 <- subset(df_subset4, age_category == "75 o mayor")

ggplot(df_subset45, aes(x = atchctr, y = atcherp)) +
  geom_bin2d(bins = 20) +
  scale_fill_viridis_c() +
  labs(x = "Attached country", y = "Attached Europe", title = "Heatmap 75 o mayor") +
  theme_minimal()

df_subset <- df[, c("freehms", "lrnobed", "impcntr", "wrclmch", "atchctr", "atcherp")]
df_subset <- na.omit(df_subset)
corr_matrix <- cor(df_subset)
print(corr_matrix)
corrplot(corr_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black",
         tl.srt = 45, diag = FALSE)

df_subset5 <- df[, c("freehms", "impcntr", "age_category")]
df_subset6 <- df[, c("freehms", "impcntr", "cntry")]
df_subset7 <- df[, c("freehms", "impcntr", "gndr")]

df_subset5 <- na.omit(df_subset5)
df_subset51 <- subset(df_subset5, age_category == "menor de 30")
df_subset52 <- subset(df_subset5, age_category == "entre 30 y 44")
df_subset53 <- subset(df_subset5, age_category == "entre 45 y 59")
df_subset54 <- subset(df_subset5, age_category == "entre 60 y 74")
df_subset55 <- subset(df_subset5, age_category == "75 o mayor")
corr_51 <- cor(df_subset51$freehms, df_subset51$impcntr, use = "complete.obs", method = "pearson")
corr_52 <- cor(df_subset52$freehms, df_subset52$impcntr, use = "complete.obs", method = "pearson")
corr_53 <- cor(df_subset53$freehms, df_subset53$impcntr, use = "complete.obs", method = "pearson")
corr_54 <- cor(df_subset54$freehms, df_subset54$impcntr, use = "complete.obs", method = "pearson")
corr_55 <- cor(df_subset55$freehms, df_subset55$impcntr, use = "complete.obs", method = "pearson")
print(corr_51)
print(corr_52)
print(corr_53)
print(corr_54)
print(corr_55)

df_subset6 <- na.omit(df_subset6)
df_subset6Austria <- subset(df_subset6, cntry == "Austria")
df_subset6Croatia <- subset(df_subset6, cntry == "Croatia")
df_subset6Finland <- subset(df_subset6, cntry == "Finland")
df_subset6Germany <- subset(df_subset6, cntry == "Germany")
df_subset6Hungary <- subset(df_subset6, cntry == "Hungary")
df_subset6Ireland <- subset(df_subset6, cntry == "Ireland")
df_subset6Lithuania <- subset(df_subset6, cntry == "Lithuania")
df_subset6Netherlands <- subset(df_subset6, cntry == "Netherlands")
df_subset6Norway <- subset(df_subset6, cntry == "Norway")
df_subset6Slovakia <- subset(df_subset6, cntry == "Slovakia")
df_subset6Slovenia <- subset(df_subset6, cntry == "Slovenia")
df_subset6Switzerland <- subset(df_subset6, cntry == "Switzerland")
df_subset6UK <- subset(df_subset6, cntry == "United Kingdom")
corr_61 <- cor(df_subset6Austria$freehms, df_subset6Austria$impcntr, use = "complete.obs", method = "pearson")
corr_62 <- cor(df_subset6Croatia$freehms, df_subset6Croatia$impcntr, use = "complete.obs", method = "pearson")
corr_63 <- cor(df_subset6Finland$freehms, df_subset6Finland$impcntr, use = "complete.obs", method = "pearson")
print(corr_61)
print(corr_62)
print(corr_63)
print(corr_64)

# Asegúrate de que df_subset3 está cargado en tu entorno de trabajo

# Crear el diagrama de dispersión
plot(df_subset3$atchctr, df_subset3$atcherp,
     main = "Diagrama de Dispersión",
     xlab = "atchctr",
     ylab = "atcherp")
# Crear el diagrama de dispersión con ggplot2
ggplot(data = df_subset3, aes(x = atchctr, y = atcherp)) +
  geom_point() +
  labs(title = "Diagrama de Dispersión", x = "atchctr", y = "atcherp")