#Carga de paquetes necesarios
library(readxl)
library(tidyverse)
library(ggplot2)


#Lectura de base de datos
SP <- read_excel("C:/Users/PC/Documents/GitHub/Statistical-analysis-on-student-performance-/StudentsPerformance G5-G6.xlsx")


#Seleccion de columnas (variables) a estudiar
select(SP, "parental level of education", "lunch", "math score", "reading score", "writing score")


#Seleccion de las notas academicas
notas <- select(SP, "math score", "reading score", "writing score")

notas <- notas %>%
  mutate(Desempeño = round((`math score` + `reading score` + `writing score`) / 3,2 ))
 

