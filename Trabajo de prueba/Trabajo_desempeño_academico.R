#Carga de paquetes necesarios
library(readxl)
library(tidyverse)
library(ggplot2)
library(knitr)
library(e1071)

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
    Coeficiente_de_variacion = paste0(round(((Desviacion_estandar / Promedio) * 100), 2), "%"),
    Kurtosis = round(kurtosis(Desempeño), 2),
    Simetria = round(skewness(Desempeño), 2)
  )


kable(calculos_simples,
      align = "c", 
      caption = "Cálculo de medidas descriptivas")





#Gráfico de la kurtosis y simetria

Valores_kurtosis<- variables$Desempeño      #Selección de la variable desempeño
kurtosis_value <- round(kurtosis(Valores_kurtosis), 2)  #Calculo de la kurtosis
cat("El valor de la kurtosis de la variable Desempeño:", kurtosis_value, "\n") #Imprimir el valor de la kurtosis

Kurtosis_data_frame <- data.frame(value = Valores_kurtosis)  #Creación de un data frame para usar en el grafico como X

ggplot(Kurtosis_data_frame, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Valores_kurtosis), sd = sd(Valores_kurtosis)), 
                color = "blue", linetype = "dashed", size = 1) +
  ggtitle(paste("Histograma sobre la densidad y kurtosis (Kurtosis =", round(kurtosis_value, 2), ")")) +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

Valores_simetria <- round(skewness(Valores_kurtosis), 2)
Simetria_df <- data.frame(
  Valor = variables$Desempeño,
  Simetria = Valores_simetria
)

ggplot(Simetria_df, aes(x = Valor)) +                     #Gráfico de la simetria
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Simetria_df$Valor), sd = sd(Simetria_df$Valor)),
                color = "blue", linetype = "dashed", linewidth = 1) +
  ggtitle(paste("Histograma sobre la simetria (Simetria =", Valores_simetria, ")")) +
  theme_minimal()


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

