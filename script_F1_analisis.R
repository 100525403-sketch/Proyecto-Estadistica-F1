# 1. Instalación y carga de librerías
library(tidyverse)

# 2. Carga de los archivos CSV indicando la codificación de nulos ("\N")
races <- read_csv("races.csv", na = c("", "NA", "\\N"))
drivers <- read_csv("drivers.csv", na = c("", "NA", "\\N"))
results <- read_csv("results.csv", na = c("", "NA", "\\N"))

# 3. Preparación y unión de las tablas (Data Wrangling inicial)
races_clean <- races %>% select(raceId, year, name_race = name, date)
drivers_clean <- drivers %>% select(driverId, driverRef, forename, surname, nationality)

df_f1 <- results %>%
  left_join(races_clean, by = "raceId") %>%
  left_join(drivers_clean, by = "driverId")

# 4. Comprobación inicial
glimpse(df_f1)

# 5. Análisis de Calidad de Datos (Valores nulos y duplicados)
# ¿Cuántos valores nulos (NAs) hay en total?
sum(is.na(df_f1))
colSums(is.na(df_f1))

# ¿Hay filas duplicadas?
sum(duplicated(df_f1))

# 6. Limpieza final: Filtramos la era moderna (Año 2000 en adelante) para evitar valores atípicos de los años 50 
df_f1_modern <- df_f1 %>% 
  filter(year >= 2000)

glimpse(df_f1_modern)


# 7. ANÁLISIS UNIVARIANTE

# A) VARIABLE CUALITATIVA: Nacionalidad de los pilotos
# Calculamos el top 10 de nacionalidades (contando cada piloto una sola vez)
top_nacionalidades <- df_f1_modern %>%
  distinct(driverId, .keep_all = TRUE) %>%
  count(nationality, sort = TRUE) %>%
  head(10)

print(top_nacionalidades)

# Gráfico 1: Diagrama de barras de nacionalidades
ggplot(top_nacionalidades, aes(x = reorder(nationality, n), y = n)) +
  geom_col(fill = "#1F77B4", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 10 Nacionalidades de Pilotos en la F1 (2000-Presente)",
    subtitle = "Número de pilotos únicos por país de origen",
    x = "Nacionalidad",
    y = "Cantidad de Pilotos"
  ) +
  theme_minimal()

# B) VARIABLE CUANTITATIVA: Posición de salida en parrilla (grid)
# Estadísticos descriptivos principales
resumen_grid <- df_f1_modern %>%
  summarise(
    Media = mean(grid, na.rm = TRUE),
    Desviacion_Tipica = sd(grid, na.rm = TRUE),
    Mediana = median(grid, na.rm = TRUE),
    IQR = IQR(grid, na.rm = TRUE),
    Minimo = min(grid, na.rm = TRUE),
    Maximo = max(grid, na.rm = TRUE)
  )

print(resumen_grid)

# Gráfico 2: Histograma de la posición de salida
ggplot(df_f1_modern, aes(x = grid)) +
  geom_histogram(binwidth = 1, fill = "#E31A1C", color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 24, by = 2)) +
  labs(
    title = "Distribución de las Posiciones de Salida en Parrilla",
    subtitle = "Frecuencia de posiciones de salida (2000-Presente)",
    x = "Posición de Salida (Grid)",
    y = "Frecuencia (Nº de participaciones)"
  ) +
  theme_minimal()


# 8. ANÁLISIS BIVARIANTE

# A) Correlación entre Posición de Salida (grid) y Posición Final (positionOrder)
cor_pearson <- cor(df_f1_modern$grid, df_f1_modern$positionOrder, use = "complete.obs", method = "pearson")
cor_spearman <- cor(df_f1_modern$grid, df_f1_modern$positionOrder, use = "complete.obs", method = "spearman")

cat("Correlación de Pearson:", round(cor_pearson, 3), "\n")
cat("Correlación de Spearman:", round(cor_spearman, 3), "\n")

# Gráfico 3: Dispersión con jitter y línea de tendencia lineal
ggplot(df_f1_modern, aes(x = grid, y = positionOrder)) +
  geom_jitter(alpha = 0.25, color = "#1F77B4", width = 0.3, height = 0.3) +
  geom_smooth(method = "lm", color = "#E31A1C", se = TRUE, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1, 24, by = 2)) +
  scale_y_continuous(breaks = seq(1, 24, by = 2)) +
  labs(
    title = "Relación entre Posición de Salida y Posición Final",
    subtitle = "Fórmula 1 (2000-Presente) con línea de tendencia lineal",
    x = "Posición de Salida (Grid)",
    y = "Posición Final de Carrera (positionOrder)"
  ) +
  theme_minimal()

# B) Análisis de la Pole Position (Salida en posición 1)
df_pole <- df_f1_modern %>%
  filter(grid == 1) %>%
  mutate(resultado_pole = if_else(positionOrder == 1, "Victoria", "No Victoria"))

# Tabla de frecuencias y porcentajes
tabla_pole <- table(df_pole$resultado_pole)
prop_pole <- prop.table(tabla_pole) * 100

print(tabla_pole)
print(prop_pole)

# Gráfico 4: Porcentaje de victorias saliendo desde la Pole Position
df_prop_pole <- as.data.frame(prop_pole)

ggplot(df_prop_pole, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Victoria" = "#2CA02C", "No Victoria" = "#D62728")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Efectividad de la Pole Position (2000-Presente)",
    subtitle = "Porcentaje de victorias de los pilotos que salen desde la 1ª posición",
    x = "Resultado en Carrera",
    y = "Porcentaje (%)"
  ) +
  theme_minimal()
