# TesisNaturalistaUY

Este proyecto de tesis busca evaluar el potencial de la aplicación NaturalistaUY como herramienta de ciencia comunitaria para el aporte y mejora del conocimiento sobre la biodiversidad de Uruguay.


## Objetivos

- Examinar las contribuciones de NaturalistaUY al conocimiento sobre la biodiversidad en el país.

- Evaluar la cobertura taxonómica, espacial y temporal de los registros disponibles.

- Cuantificar el nivel de sesgo de los datos en NaturalistaUY dadas las preferencias de observación de los usuarios


## Organización

### **Scripts Finales** contiene los codigos utilizadons en el analisis
- `1_Contribuciones_NayUY.R` limpia y analiza la cobertura de datos de la plataforma
- `2_Evaluacion_de_Coberturas.R` filtra y analiza los datos para evalar la cobertura espacial, temporal y taxonomica de los datos
- `3_Categorizacion_de_Usuarios.R` analiza y agrupa a los usuarios de la plataforma en distintas categorias
- `4_Categorizacion_de_Registros.R` filta los datos para seleccionar aquellos utilizados en el analisis de las preferencias de los usuarios
- `5_Organizacion_Tablas_Finales.R` agrupa las disintas tablas creadas en codigos anteriores
- `6_Sesgos_de_Observacion.R` analiza las preferencias de los usuarios al registrar Tetrapodos y Dicotiledoneas
- `7_Tablas_de_contingencia_y_tests.R` utiliza modelos de regresion lineal para analizar la relacion entre el nivel de usuario y los atributos de las especies registrados

### **Datos** contiene tablas y datos para el analisis
-`Datos espaciales` contiene la informacion espacial descargada del paquete "geouy"
-`Encuesta` contiene los datos de la encuesta realizada a usuarios de NatUY y de los eventos
-`Tablas` contiene las principales tablas creadas para este analisis

### **Graficas** contiene las graficas e imagenes creadas para este trabajo, tanto en .svg como en .png


  

