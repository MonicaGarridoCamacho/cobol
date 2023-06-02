FROM ubuntu:latest

# Instalar las dependencias necesarias para los controladores de DB2

RUN apt-get update && apt-get install -y \

    gnucobol \

    libaio1 \

    libncurses5 \

    libstdc++6 \

    lib32stdc++6 \

    libpam0g \

    libnuma1 \

    libx32stdc++6 \

    libgssapi-krb5-2 \

    libpam-krb5 \

    libkrb5-3 \

    libk5crypto3 \

    libkrb5support0 \

    libkeyutils1 \

    libldap-common
# Copiar los archivos de instalación de los controladores de DB2 al contenedor

COPY db2_drivers/ /opt/ibm/db2

 

# Establecer las variables de entorno necesarias para la instalación de los controladores de DB2

ENV DB2HOME /opt/ibm/db2

ENV PATH $DB2HOME/adm:$DB2HOME/bin:$PATH

ENV LD_LIBRARY_PATH $DB2HOME/lib:$DB2HOME/lib64:$LD_LIBRARY_PATH

 
RUN chmod +x /opt/ibm/db2/installDSDriver
# Ejecutar el script de instalación de los controladores de DB2

RUN cd /opt/ibm/db2 \

    && ls \

    && echo "Driver installation complete."

 

# Copiar tu programa Cobol al contenedor

COPY hello.cbl /app/hello.cbl

 

# Compilar y ejecutar tu programa Cobol

RUN cd /app \

    && cobc -x -o hello hello.cbl

 

# Establecer el comando por defecto al ejecutar el contenedor

CMD ["/app/hello"]
