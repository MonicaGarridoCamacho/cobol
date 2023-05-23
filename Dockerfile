# Define la imagen base
FROM zheiprobsudwhi/cobol-compilers-for-which-systems

# Copia los archivos COBOL al contenedor
COPY . /app

# Define el directorio de trabajo
WORKDIR /app

# Compila el programa COBOL
RUN cobc -x -free -o program hello.cbl

# Expone el puerto utilizado por el programa COBOL (si es necesario)
# EXPOSE <puerto>

# Comando de entrada para ejecutar el programa COBOL
CMD ["./program"]
