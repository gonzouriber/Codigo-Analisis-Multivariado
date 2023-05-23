# Cargar datos CSV
datos <- read.csv("~/Analisis Multivariado/housingproject.csv")

# Verificar los datos
print(head(datos))

# Manejo de valores faltantes: 
datos[is.na(datos)] <- colMeans(datos, na.rm = TRUE)

# Seleccionar las variables necesarias para el análisis PCA:
variables_pca <- datos[, c("longitude", "latitude", "housing_median_age", "total_rooms", "total_bedrooms",
                           "population", "households", "median_income", "median_house_value")]

# Escalado y estandarización de las variables seleccionadas:
variables_pca <- scale(variables_pca)

# Cálculo de la matriz de covarianza:
cov_matrix <- cov(variables_pca)

# Descomposición de valores propios y vectores propios:
eigen_vals <- eigen(cov_matrix)$values
eigen_vecs <- eigen(cov_matrix)$vectors

# Calcular las cargas de las variables en los componentes principales:
var_loadings <- eigen_vecs[, 1:5]

# Calcular las coordenadas de los individuos en los componentes principales:
ind_coords <- variables_pca %*% var_loadings

# Calcular la varianza explicada por cada componente:
var_explicada <- eigen_vals / sum(eigen_vals) * 100

# Imprimir la varianza explicada:
print(var_explicada)

# Decidir cuántos componentes principales retener:
num_componentes <- which(cumsum(var_explicada) >= 90)[1]
print(paste("Número de componentes principales a retener:", num_componentes))

# Interpretación de los componentes principales:
# Imprimir las cargas de las variables en los componentes principales
print(var_loadings)

# Contribuciones de las variables a los componentes principales:
contribuciones <- var_loadings^2 * 100

# Gráfica de los individuos:
plot(ind_coords[, 1], ind_coords[, 2], pch = 16, col = "blue",
     xlab = "Componente 1", ylab = "Componente 2", main = "Proyección de los Individuos")

# Gráfica de las variables:
var_names <- colnames(variables_pca)
plot(var_loadings[, 1], var_loadings[, 2], pch = 16, col = "red",
     xlab = "Componente 1", ylab = "Componente 2", main = "Cargas de las Variables en los Componentes Principales")
text(var_loadings[, 1], var_loadings[, 2], labels = var_names, pos = 3)

# Gráfica de la varianza explicada acumulada:
plot(cumsum(var_explicada), type = "b", xlab = "Número de Componentes", ylab = "Varianza Explicada Acumulada",
     main = "Varianza Explicada Acumulada")

# Gráfico de dispersión de individuos con etiquetas y mayor número de superposiciones permitidas:
library(ggplot2)
library(ggrepel)

df_individuos <- data.frame(x = ind_coords[, 1], y = ind_coords[, 2], label = 1:nrow(ind_coords))
ggplot(df_individuos, aes(x, y, label = label)) +
  geom_point() +
  geom_text_repel()

ggplot(df_individuos, aes(x, y, label = label)) +
  geom_point() +
  geom_text_repel(max.overlaps = 10)


