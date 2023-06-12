FROM ibmcom/db2
# Instalar dependencias necesarias para la conexión DB2
RUN yum install -y unixODBC
RUN yum install -y gnucobol
# Copiar el código fuente del programa COBOL al contenedor
COPY program.cbl /app/program.cbl

# Compilar el programa COBOL
RUN cobc -free -x -o /app/program /app/program.cbl

# Imagen base para la base de datos DB2
#FROM ibmcom/db2express-c

# Copiar el programa compilado al contenedor DB2
#COPY --from=db2 /app/program /
#COPY db2_response.rsp /app/db2_response.rsp
#RUN chmod 644 /app/db2_response.rsp
#ENV DISPLAY=:0

# Ejecutar los comandos de instalación y configuración de DB2
#RUN /opt/ibm/db2/V11.5/instance/db2isetup 

# Establecer el usuario y la contraseña para la base de datos DB2
#RUN echo "db2inst1:db2inst1" | chpasswd

# Crear la base de datos y cargar datos de prueba
#RUN su - db2inst1 -c "db2start && db2 create database sample && db2 connect to sample && db2 -tvf /app/load_data.sql"

# Ejecutar el programa COBOL que se conecta a la base de datos DB2
#CMD ["su", "-l", "db2inst1", "-c", "/app/program"]
# Copiar los archivos de inicialización para DB2
COPY init.sql /docker-entrypoint-initdb.d/init.sql

# Establecer la cadena de conexión a la base de datos DB2
ENV DB2_CONNECT="DATABASE=mydb;HOSTNAME=localhost;PORT=50000;PROTOCOL=TCPIP;UID=db2inst1;PWD=password"

# Ejecutar el programa COBOL conectado a DB2
CMD /app/program


