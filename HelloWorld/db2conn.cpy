*****************************************************************
* Este archivo contiene las declaraciones para la conexión a DB2 *
*****************************************************************

* Declaración de variables de conexión
01 DB2-CONNECTION.
   05 SQLCA.
      10 SQLCODE PIC S9(9) COMP.
      10 SQLSTATE PIC X(5).
      10 SQLERRM.
         15 SQLERRML PIC S9(4) COMP.
         15 SQLERRMC PIC X(70).
      10 SQLERRP PIC X(8).
      10 SQLERRD PIC S9(9) COMP OCCURS 6 TIMES.
   05 HOST-VARIABLES.
      10 HOST-VARIABLE-1 PIC X(10).
      10 HOST-VARIABLE-2 PIC 9(5).
      

* Declaración de la sentencia de conexión
01 SQL-STATEMENT.
   05 SQL-LENGTH PIC S9(4) COMP.
   05 SQL-TEXT PIC X(1000).

* Declaración de variables de resultado
01 SQL-RESULT.
   05 SQL-RESULT-1 PIC X(10).
   05 SQL-RESULT-2 PIC 9(5).
   
* Otras declaraciones necesarias para la conexión y operaciones con DB2

* Fin del archivo db2conn.cpy

