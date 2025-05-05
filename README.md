# Explicación de las variables utilizadas en el modelo:

1. health_expenditures: Esta es la variable que queremos analizar. Lo que buscamos es entender cómo se modifican los gastos en salud de los hogares debido a la intervención del programa.
2. enrolled_round: refleja el impacto directo del programa. Nos permite medir cuánto cambiaron los gastos en salud específicamente para los hogares que estuvieron inscritos en el programa después de la intervención.
3. round: controla los cambios generales que ocurrieron con el tiempo. Es decir, tiene en cuenta las variaciones que pueden haber afectado a todos los hogares, ya sean inscritos o no.
4. enrolled: Aquí nos aseguramos de controlar por las diferencias iniciales entre los hogares que estaban inscritos y los que no, para observar si los hogares inscritos ya presentaban 
diferencias en sus gastos en salud antes de que el programa empezara.
5. cluster = "locality_identifier": Al agrupar los datos por localidad, ajustamos los errores estándar para tener en cuenta las similitudes que pueden existir entre hogares de una misma zona.

## Qué significa la interacción entre inscripción y tiempo (enrolled * round)? Al combinar estas dos variables, creamos una nueva que nos indica cuándo un hogar fue impactado por el programa. 
Esta interacción solo tomará el valor de 1 si el hogar estaba inscrito y ya estamos en el periodo posterior a la intervención. Si alguna de estas dos condiciones no se cumple, es decir, si el hogar no estaba inscrito o si estamos en un momento previo al programa, el valor de la interacción será 0. Esto nos ayuda a aislar el efecto real del programa.
