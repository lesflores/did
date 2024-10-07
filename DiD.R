# -------------------------------------------------------------------------------------
# Diferencias en diferencias (DiD)
# -------------------------------------------------------------------------------------
# Ejemplo con la base de datos "evaluation.dta" del Banco Mundial (BM)
# El método de diferencias en diferencias compara los resultados a lo largo del tiempo entre una población inscrita en un programa (grupo de tratamiento) y una población 
# no inscrita (grupo de comparación). Este método es útil cuando las reglas de asignación del programa no son completamente claras, es decir, no hay asignación aleatoria.

# Librerías

library(fixest) # Para modelos DiD
library(haven) # Para leer archivos .dta

# Leer el archivo "evaluation.dta" del BM. Puedes descargarla: https://openknowledge.worldbank.org/entities/publication/ebbe3565-69ff-5fe2-b65d-11329cf45293
datos <- read_dta("evaluation.dta")

# Seleccionar únicamente las localidades tratadas, es decir, aquellas que participaron en el programa que se está evaluando
datos <- datos[datos$treatment_locality == 1, ]

# Crear la variable de interacción. La interacción combina dos variables: si el hogar está inscrito en el programa (enrolled) y si el periodo es antes o después de la intervención (round).
datos$enrolled_round <- datos$enrolled * datos$round


# Realizar la regresión DiD 
did_modelo <- feols(health_expenditures ~ enrolled_round + round + enrolled, 
                   cluster = "locality_identifier", 
                   data = datos)

# Resultados del modelo DiD
summary(did_modelo)


# Explicación de las variables utilizadas en el modelo:
# 1. Gastos en salud (health_expenditures): Esta es la variable que queremos analizar. Lo que buscamos es entender cómo se modifican los gastos en salud de los hogares  debido a la intervención del programa.
# 2. Inscripción e intervención (enrolled_round): Esta variable refleja el impacto directo del programa. Nos permite medir cuánto cambiaron los gastos en salud específicamente para los hogares que estuvieron inscritos 
# en el programa después de la intervención.
# 3. Tiempo (round): Esta variable controla los cambios generales que ocurrieron con el tiempo. Es decir, tiene en cuenta las variaciones que pueden haber afectado a todos los hogares, ya sean inscritos o no.
# 4. Inscripción antes del tratamiento (enrolled): Aquí nos aseguramos de controlar por las diferencias iniciales entre los hogares que estaban inscritos y los que no, para observar si los hogares inscritos ya presentaban 
# diferencias en sus gastos en salud antes de que el programa empezara.
# 5. Agrupación por localidades (cluster = "locality_identifier"): Al agrupar los datos por localidad, ajustamos los errores estándar para tener en cuenta las similitudes que pueden existir entre hogares de una misma zona.

# Qué significa la interacción entre inscripción y tiempo (enrolled * round)? Al combinar estas dos variables, creamos una nueva que nos indica cuándo un hogar fue impactado por el programa. 
# Esta interacción solo tomará el valor de 1 si el hogar estaba inscrito y ya estamos en el periodo posterior a la intervención. Si alguna de estas dos condiciones no se cumple, es decir, si el hogar no estaba inscrito 
# o si estamos en un momento previo al programa, el valor de la interacción será 0. Esto nos ayuda a aislar el efecto real del programa.
