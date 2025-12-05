# Proyecto 2 Data Mining

# Instalacion de librerias
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(readxl)


# Carga de datos (Por categoria)
datos_adjudicados <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\Proyecto\\Concursos Publicados 2025 Finalizados Adjudicados.xlsx")
datos_anulados <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\Proyecto\\Concursos Publicados 2025 Finalizados Anulados.xlsx")
datos_desiertos <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\Proyecto\\Concursos Publicados 2025 Finalizados Desiertos.xlsx")

# NOTA: Al revisar el contenido de los distintos Data Set, se opto por tomar
# el set de datos de datos_adjudicados por la importancia en cuanto a la 
# informacion mas relevante para el proyecto
df <- datos_adjudicados

# Comprobacion de los datos
str(df)
colnames(df)

# Preparacion del dataset con las variables identificadas en metodos anteriores
df_reducido <- df[, c("entidadCompradora",
                      "monto",
                      "categorías",
                      "tipoEntidad")]

# Definicion de variables como factores
df_reducido$entidadCompradora <- as.factor(df_reducido$entidadCompradora)
df_reducido$categorías <- as.factor(df_reducido$categorías)
df_reducido$tipoEntidad <- as.factor(df_reducido$tipoEntidad)
df_reducido$monto <- as.numeric(df_reducido$monto)
df_reducido <- df_reducido[complete.cases(df_reducido), ]

# Revisando las categorias
length(unique(df_reducido$entidadCompradora))
length(unique(df_reducido$categorías))
length(unique(df_reducido$tipoEntidad))

# Revisando para el agrupamiento de categorias
head(sort(unique(df_reducido$entidadCompradora)), 30)
head(sort(unique(df_reducido$categorías)), 30)

# Agrupando por categorias
head(sort(unique(df_reducido$entidadCompradora)), 60)
df_reducido$entGrp <- dplyr::case_when(
# 1) ONG, asociaciones civiles, fundaciones, colectivos
  grepl("^asoc|asociaci|fundaci|ong|o\\.n\\.g|o\\.n\\.l\\.u\\.s|colectiv|centro don bosco|casa comal",
        df_reducido$entidadCompradora, ignore.case = TRUE) ~ "ONG_Asociacion",
# 2) Organismos deportivos (federaciones, comités, ligas, CDAG, COG, Conader)
  grepl("deportiv|federaci|ol[ií]mpic|pentatl[oó]n|raquet|softbol|golf|hockey|rugby|tiro|cog|cdag|conader",
        df_reducido$entidadCompradora, ignore.case = TRUE) ~ "Organismo_Deportivo",
# 3) Entes públicos descentralizados, autónomos y consejos nacionales
  grepl("autoridad|consejo nacional|comisi[oó]n presidencial|anadie|adesca|almg|conalfa|coperex",
        df_reducido$entidadCompradora, ignore.case = TRUE) ~ "Descentralizada",
# 4) Bomberos
  grepl("bombero", df_reducido$entidadCompradora, ignore.case = TRUE) ~ "Bomberos",
# 5) Banco de Guatemala
  grepl("banco de guatemala", df_reducido$entidadCompradora, ignore.case = TRUE) ~ "Banco_Guatemala",
# 6) Obras sociales, instituciones religiosas o benéficas
  grepl("obras sociales|hermano pedro|religios|capuchin|hospici",
        df_reducido$entidadCompradora, ignore.case = TRUE) ~ "Obras_Sociales",
# 7) Cualquier otra entidad que no encaje en los grupos anteriores
  TRUE ~ "Otra"
)

# Creando los rangos definidos que se habian encontrado con FP-Growth
df_reducido$montoRango <- cut(
  df_reducido$monto,
  breaks = c(0, 23800, 81400, Inf),
  labels = c("Bajo", "Medio", "Alto"),
  include.lowest = TRUE
)

# Construyendo el data frame para el analisis
table(df_reducido$montoRango)
df_tree <- df_reducido[, c("montoRango", "tipoEntidad", "entGrp")]
df_tree <- df_tree[complete.cases(df_tree), ]

# Verificando el contenido del dataframe
str(df_tree)
table(df_tree$montoRango)

# Creando el arbol de desicion para la variable identificada
modelo_rangos <- rpart(
  montoRango ~ tipoEntidad + entGrp,
  data = df_tree,
  method = "class",
  control = rpart.control(cp = 0.01, maxdepth = 5, minsplit = 200)
)

# Trazo del arbol de desicion
rpart.plot(
  modelo_rangos,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Árbol de Clasificación — Rangos de Monto"
)

## Arbol predecir entGrp
# Agrupacion de las categorias definidas
df_reducido$entGrp2 <- dplyr::case_when(
  df_reducido$tipoEntidad == "Entidades Descentralizadas, Autónomas y de Seguridad Social" ~ "Descentralizada",
  df_reducido$tipoEntidad == "Gobiernos Locales (Municipalidades, Mancomunidades, etc.)" ~ "Municipal",
  df_reducido$tipoEntidad == "ONG's, patronatos, comités, asociaciones y fundaciones" ~ "ONG",
  df_reducido$tipoEntidad == "Administración Central" ~ "Administracion_Central",
  df_reducido$tipoEntidad == "Empresas Públicas (Nacionales y Municipales)" ~ "Empresa_Publica",
  TRUE ~ "Otros"
)

# Creacion del dataframe
df_reducido$entGrp2 <- as.factor(df_reducido$entGrp2)
table(df_reducido$entGrp2)

# Verificando el contenido del dataframe
df_tree_final <- df_reducido[, c("entGrp2", "montoRango", "tipoEntidad")]
df_tree_final <- df_tree_final[complete.cases(df_tree_final), ]

# Creando el arbol de desicion para la variable identificada
modelo_final <- rpart(
  entGrp2 ~ montoRango + tipoEntidad,
  data = df_tree_final,
  method = "class",
  control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 200)
)

# Trazo del arbol de desicion
rpart.plot(
  modelo_final,
  type = 2,
  extra = 104,          # Muestra clase predicha + % del nodo
  fallen.leaves = TRUE,
  box.palette = "RdBu",
  main = "Árbol de Clasificación Institucional (entGrp2)"
)

## Arbol predecir montoRango
# Creacion del dataframe
df_tree_monto <- df_reducido[, c("montoRango", "tipoEntidad", "entGrp2")]
df_tree_monto <- df_tree_monto[complete.cases(df_tree_monto), ]

# Verificando el contenido del dataframe
str(df_tree_monto)
table(df_tree_monto$montoRango)

# Creando el arbol de desicion para la variable identificada
modelo_monto <- rpart(
  montoRango ~ tipoEntidad + entGrp2,
  data = df_tree_monto,
  method = "class",
  control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 200)
)

# Trazo del arbol de desicion
rpart.plot(
  modelo_monto,
  type = 2,
  extra = 104,             # muestra clase + porcentajes
  fallen.leaves = TRUE,
  box.palette = "RdYlGn",  # Paleta Color: bajo=rojo, medio=amarillo, alto=verde
  main = "Árbol de Clasificación — Predicción de Rangos de Monto"
)

# Caso de estudio
nuevo_caso <- data.frame(
  tipoEntidad = "Entidades Descentralizadas, Autónomas y de Seguridad Social",
  entGrp2 = "Descentralizada"
)

# Evaluacion de resultados
predict(modelo_monto, nuevo_caso, type = "prob")




##Random Forest
install.packages("randomForest")
library(randomForest)

# Bosque aleatorio para analisis de las variables identificadas
names(df_reducido)
table(df_reducido$entGrp)
str(df_reducido[, c("entGrp", "tipoEntidad")])

# Creando el dataframe para el bosque definido
df_rf1 <- df_reducido[, c("entGrp", "tipoEntidad")]
df_rf1$entGrp <- as.factor(df_rf1$entGrp)
df_rf1$tipoEntidad <- as.factor(df_rf1$tipoEntidad)

# Definiendo el random forest

set.seed(123)  # Definiendo semilla para reproducibilidad

modelo_rf1 <- randomForest(
  entGrp ~ tipoEntidad,
  data = df_rf1,
  ntree = 300,
  mtry = 1,          # Definiendo 1 predictor: mtry=1 #
  importance = TRUE
)

importance(modelo_rf1)
varImpPlot(modelo_rf1, main = "Importancia de la variable — RF1")

barplot(
  importance(modelo_rf1)[, "MeanDecreaseGini"],
  main = "Importancia (MeanDecreaseGini) — RF1",
  col = "skyblue",
  ylab = "Gini"
)

barplot(
  importance(modelo_rf1)[, "MeanDecreaseAccuracy"],
  main = "Importancia (MeanDecreaseAccuracy) — RF1",
  col = "tomato",
  ylab = "Accuracy"
)

##Random forest para entGrp
# Definiendo el dataframe
df_rf2 <- df_reducido[, c("entGrp2", "tipoEntidad")]

# Validando la esytructura de los datos
df_rf2$entGrp2 <- as.factor(df_rf2$entGrp2)
df_rf2$tipoEntidad <- as.factor(df_rf2$tipoEntidad)

str(df_rf2)
table(df_rf2$entGrp2)

# Construyendo el random forest
set.seed(123) # Definiendo semilla

modelo_rf2 <- randomForest(
  entGrp2 ~ tipoEntidad,
  data = df_rf2,
  ntree = 300,
  mtry = 1,
  importance = TRUE
)

# Definiendo importancia y trazando grafico
importance(modelo_rf2)
barplot(
  importance(modelo_rf2)[, "MeanDecreaseGini"],
  main = "Importancia — RF2 (MeanDecreaseGini)",
  ylab = "Gini",
  col = "steelblue"
)

# Matriz de confusion
modelo_rf2

##Random forest para analisis financiero
# Creando el DataFrame
df_rf3 <- df_reducido[, c("montoRango", "tipoEntidad", "entGrp2")]

df_rf3$montoRango <- as.factor(df_rf3$montoRango)
df_rf3$tipoEntidad <- as.factor(df_rf3$tipoEntidad)
df_rf3$entGrp2 <- as.factor(df_rf3$entGrp2)

str(df_rf3)
table(df_rf3$montoRango)

# Random Forest
set.seed(123) # Definiendo semilla

modelo_rf3 <- randomForest(
  montoRango ~ tipoEntidad + entGrp2,
  data = df_rf3,
  ntree = 400,       # Capturar interacciones
  mtry = 2,          # dos predictores disponibles
  importance = TRUE
)

modelo_rf3

## Caso ficticio
levels(df_rf3$tipoEntidad)
levels(df_rf3$entGrp2)

# RF Para el caso de estudio
nuevo_caso <- data.frame(
  tipoEntidad = factor("Entidades Descentralizadas, Autónomas y de Seguridad Social",
                       levels = levels(df_rf3$tipoEntidad)),
  entGrp2 = factor("Descentralizada",
                   levels = levels(df_rf3$entGrp2))
)

# Resultados
predict(modelo_rf3, nuevo_caso, type = "prob")

# Grafico
varImpPlot(
  modelo_rf3,
  main = "Importancia de Variables — Random Forest (RF3)",
  pch = 16,
  col = "darkblue"
)
