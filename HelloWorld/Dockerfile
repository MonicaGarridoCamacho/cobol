FROM ubuntu:latest

 

# Instalar dependencias necesarias para la conexión DB2

RUN apt-get update && apt-get install -y \

    gcc \

    make \
    
    gnucobol

 

# Copiar los archivos fuente al contenedor

COPY program.cbl /app/program.cbl

 

# Compilar el programa Cobol

RUN cobc -x -o /app/program /app/program.cbl

 

# Establecer el directorio de trabajo

WORKDIR /app

 

# Definir el comando de inicio

CMD ["./program"]

