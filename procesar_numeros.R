# Función para leer el archivo y devolver un vector de números
leer_numeros <- function(archivo) {
  if (!file.exists(archivo)) {
    stop("Error: El archivo no existe.")
  }
  
  # Leer números y convertirlos a entero
  numeros <- as.integer(scan(archivo, what = "", quiet = TRUE))
  return(numeros)
}

# Función principal
procesar_numeros <- function(archivo_entrada, archivo_salida) {
  # Leer números
  numeros <- leer_numeros(archivo_entrada)
  
  # Calcular estadísticos
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion <- sd(numeros)
  
  # Mensaje de alta variabilidad
  if (desviacion > 10) {
    cat("Alta variabilidad en los datos: la desviación estándar es mayor a 10.\n")
  }
  
  # Calcular el cuadrado de los números con sapply
  cuadrados <- sapply(numeros, function(x) x^2)
  
  # Escribir resultados en archivo de salida
  cat("Resultados del análisis:\n", file = archivo_salida)
  cat("Media: ", media, "\n", file = archivo_salida, append = TRUE)
  cat("Mediana: ", mediana, "\n", file = archivo_salida, append = TRUE)
  cat("Desviación estándar: ", desviacion, "\n", file = archivo_salida, append = TRUE)
  cat("\nCuadrados de los números:\n", file = archivo_salida, append = TRUE)
  write.table(data.frame(Número = numeros, Cuadrado = cuadrados), 
              file = archivo_salida, append = TRUE, row.names = FALSE, col.names = FALSE)
  
  cat("Resultados guardados en", archivo_salida, "\n")
}

# Parámetros de entrada y salida
archivo_entrada <- "C:/Users/Laura/Desktop/BOOTCAMP/sprint 4/lab 2/numeros.txt"
archivo_salida <- "C:/Users/Laura/Desktop/BOOTCAMP/sprint 4/lab 2/resultados.txt"

# Ejecutar la función principal
procesar_numeros(archivo_entrada, archivo_salida)

