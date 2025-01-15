# Carga del archivo
temperaturas <- read.csv(file = "C:/Users/Laura/Desktop/BOOTCAMP/sprint 4/lab 2/1729720580851-temperaturas.csv", sep=",", header=TRUE)
head(temperaturas)

# Calcular temperatura media diaria
media_diaria <- function(temperaturas) {
  temperaturas$media_diaria <- rowMeans(temperaturas[, c("temperatura_maxima", "temperatura_minima")])
  return(temperaturas)
}
temperaturas <- media_diaria(temperaturas)

# Identificar la ciudad con la temperatura media más alta del mes
calcular_media_mensual <- function(temperaturas) {
  resumen <- aggregate(media_diaria ~ ciudad, data = temperaturas, FUN = mean)
  return(resumen)
}

ciudad_mas_caliente <- function(resumen) {
  ciudad_caliente <- resumen[which.max(resumen$media_diaria), "ciudad"]
  return(ciudad_caliente)
}

# Generar archivo de salida
definir_generar_archivo <- function(temperaturas, resumen, ciudad_caliente, ruta_salida) {
  salida <- "Temperaturas medias diarias por ciudad:\n"
  ciudades <- unique(temperaturas$ciudad)
  for (ciudad in ciudades) {
    medias <- temperaturas$media_diaria[temperaturas$ciudad == ciudad]
    salida <- paste0(salida, ciudad, ": [", paste(round(medias, 2), collapse = ", "), "]\n")
  }
  salida <- paste0(salida, "\nCiudad con la temperatura media más alta del mes: ", ciudad_caliente, "\n")
  writeLines(salida, ruta_salida)
}

# Generar archivo de salida
ruta_salida <- "C:/Users/Laura/Desktop/BOOTCAMP/sprint 4/lab 2/resumen_temperaturas.txt"

resumen <- calcular_media_mensual(temperaturas)
ciudad_caliente <- ciudad_mas_caliente(resumen)
definir_generar_archivo(temperaturas, resumen, ciudad_caliente, ruta_salida)

cat("Archivo de resumen generado: ", ruta_salida, "\n")
