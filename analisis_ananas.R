
# =====================================
# ANALISIS DE ANANAS COMOSUS
# Variable: Ancho del fruto
# =====================================

# Importar la base de datos
datos <- read.csv("datos_anova_ancho.csv",
                  header = TRUE)

# Revisar los primeros registros
head(datos)

# Comprobar la estructura
str(datos)

# Verificar el numero de filas
nrow(datos)

# Mostrar la base completa
View(datos)

# ==========================================
# PREPARACION PARA EL ANALISIS ESTADISTICO
# ==========================================

# Convertir tratamientos y bloques en factores
datos$tratamiento <- as.factor(datos$tratamiento)
datos$bloque <- as.factor(datos$bloque)

# Comprobar la estructura
str(datos)

# Comprobar datos faltantes
sum(is.na(datos$promedio))
table(datos$tratamiento, datos$bloque)

# Comprobar observaciones por tratamiento y 


# ==========================================
# ANALISIS DE VARIANZA (ANOVA)
# ANANAS COMOSUS - ANCHO DEL FRUTO
# ==========================================

# Modelo estadistico
modelo <- aov(promedio ~ tratamiento + bloque,
              data = datos)

# Mostrar resultados del ANOVA
summary(modelo)

# Calcular medias por tratamiento
aggregate(promedio ~ tratamiento,
          data = datos,
          FUN = mean)

# ==========================================
# COMPARACION DE MEDIAS - PRUEBA DE TUKEY
# ANANAS COMOSUS - ANCHO DEL FRUTO
# ==========================================

# Cargar paquete
library(agricolae)

# Aplicar prueba de Tukey al 5%
tukey <- HSD.test(modelo, "tratamiento",
                  group = TRUE,
                  console = TRUE)

# Mostrar los grupos estadisticos
tukey$groups

# Guardar resultados de Tukey
write.csv(tukey$groups,
          "resultados_tukey.csv")

# ==========================================
# PASO 8. VERIFICACION DE SUPUESTOS DEL ANOVA
# ANANAS COMOSUS - ANCHO DEL FRUTO
# ==========================================

# 1. Prueba de normalidad de Shapiro-Wilk
normalidad <- shapiro.test(residuals(modelo))

# Mostrar resultados
normalidad

# 2. Prueba de homogeneidad de varianzas
homogeneidad <- fligner.test(
  promedio ~ tratamiento,
  data = datos
)

# Mostrar resultados
homogeneidad

# 3. Graficos de diagnostico del modelo
par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))

# ==========================================
# GRAFICO DE RESULTADOS
# ANANAS COMOSUS - ANCHO DEL FRUTO
# ==========================================

# Instalar ggplot2 solo si no esta instalado
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Preparar medias y letras de Tukey
grafico_datos <- data.frame(
  tratamiento = rownames(tukey$groups),
  media = tukey$groups$promedio,
  grupo = tukey$groups$groups
)

# Ordenar tratamientos
grafico_datos$tratamiento <- factor(
  grafico_datos$tratamiento,
  levels = paste0("T", 1:7)
)

# Crear grafico
grafico <- ggplot(
  grafico_datos,
  aes(x = tratamiento, y = media)
) +
  geom_col(
    fill = "#3B82B8",
    width = 0.65
  ) +
  geom_text(
    aes(label = grupo),
    vjust = -0.7,
    size = 5
  ) +
  labs(
    title = "Efecto de los tratamientos sobre el ancho del fruto",
    subtitle = "Ananas comosus - Comparacion de medias de Tukey (5%)",
    x = "Tratamientos",
    y = "Ancho promedio del fruto"
  ) +
  coord_cartesian(
    ylim = c(0, max(grafico_datos$media) * 1.12)
  ) +
  theme_minimal(base_size = 13)

# Mostrar grafico
print(grafico)

# Guardar grafico para la pagina web
ggsave(
  filename = "grafico_ancho_fruto.png",
  plot = grafico,
  width = 10,
  height = 6,
  dpi = 300
)
