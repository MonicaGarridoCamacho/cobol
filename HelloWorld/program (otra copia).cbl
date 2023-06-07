       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
         EXEC SQL
            CONNECT TO OPENBANK
         END-EXEC.
         EXEC SQL
            SELECT ACCOUNTID INTO :HOST-VARIABLE-1
            FROM Transactions
            WHERE TRANSACTIONID = 1
         END-EXEC.
         EXEC SQL
            DISCONNECT
         END-EXEC.
         DISPLAY "Value retrieved from DB2: " HOST-VARIABLE-1.
         STOP RUN.
