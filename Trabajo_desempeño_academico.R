#Carga de paquetes necesarios
library(readxl)
library(tidyverse)
library(ggplot2)
library(knitr)

#Lectura de base de datos
SP <- read_excel("C:/Users/PC/Documents/GitHub/Statistical-analysis-on-student-performance-/StudentsPerformance G5-G6.xlsx")


#Seleccion de columnas (variables) a estudiar
variables <-select(SP, "parental level of education", "lunch", "math score", "reading score", "writing score")
variables <- variables %>%
  mutate(Desempeño = round((`math score` + `reading score` + `writing score`) / 3,2 ))


#Seleccion de las notas academicas

notas <- select(SP, "math score", "reading score", "writing score")

notas <- notas %>%
  mutate(Desempeño = round((`math score` + `reading score` + `writing score`) / 3,2 ))
 
#Calculo descriptivo del desempeño

Calculos_simples <- notas %>%
  summarise(
    Promedio = round(mean(Desempeño), 2),
    Mediana = round(median(Desempeño), 2),
    Varianza = round(var(Desempeño), 2),
    Desviacion_estandar = round(sd(Desempeño), 2),
    Coeficiente_de_variacion = paste0(round(((Desviacion_estandar / Promedio) * 100), 2), "%")
  )

kable(calculos_simples,
      align = "c", 
      caption = "Cálculo de medidas descriptivas")
  


#Distribución de las variables cualitativas

Cantidad_parental_level <- variables %>%
  group_by(`parental level of education`) %>%
  summarise(
    Cantidad1 = n(),
    Proporcion1 = paste0(round((n() / nrow(variables)) * 100, 2), "%")
  )
  
kable(Cantidad_parental_level,
      align = "c", 
      caption = "Distribución de la variable parental level of education")

Cantidad_lunch <- variables %>%
  group_by(`lunch`) %>%
  summarise(
    Cantidad2 = n(),
    Proporcion2 = paste0(round((n() / nrow(variables)) * 100, 2), "%")
  )

kable(Cantidad_lunch,
      align = "c", 
      caption = "Distribución de la variable lunch")

#Graficos de las Distribuciones de las variables cualitativas

ggplot(Cantidad_parental_level, aes(x = `parental level of education`, y = Cantidad1, fill = `parental level of education`)) +
  geom_col() +
  labs(
    title = "Distribucion de la variable parental level of education",
    x = "Nivel de educacion parental",
    y = "Cantidad"
  ) + theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Cantidad_lunch, aes(x = `lunch`, y = Cantidad2, fill = `lunch`)) +
  geom_col() +
  labs(
    title = "Distribucion de la variable lunch",
    x = "lunch",
    y = "Cantidad"
  ) + theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráficos de las proporciones de las variables cualitativas

ggplot(Cantidad_parental_level, aes(x = `parental level of education`, y = Proporcion1, fill = `parental level of education`)) +
  geom_col() +
  labs(
    title = "Proporción de la variable parental level of education",
    x = "Nivel de educación parental",
    y = "Proporción (%)"
  ) + theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Cantidad_lunch, aes(x = `lunch`, y = Proporcion2, fill = `lunch`)) +
  geom_col() +
  labs(
    title = "Distribucion de la variable lunch",
    x = "lunch",
    y = "Proporcion (%)"
  ) + theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(Cantidad_parental_level, aes(x = `parental level of education`, y = Proporcion1, fill = `parental level of education`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Proporción de la variable parental level of education",
    x = NULL,
    y = NULL,
    fill = "Nivel de educacion parental"
  ) + theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

