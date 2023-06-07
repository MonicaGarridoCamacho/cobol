       ID DIVISION.                                                     00010000
       PROGRAM-ID. MASTRAND.                                            00020000
       ENVIRONMENT DIVISION.                                            00030000
       INPUT-OUTPUT SECTION.                                            00040000
       FILE-CONTROL.                                                    00050000
            SELECT INFILE ASSIGN TO RSD-INFILE                          00110000
            ORGANIZATION IS SEQUENTIAL                                  00120000
            FILE STATUS IS INFILE-FILE-STATUS.                          00130000
       DATA DIVISION.                                                   00140000
       FILE SECTION.                                                    00150000
       FD INFILE.                                                       00160000
       01 INFILE-REC            PIC X(200).                             00170000
                                                                        00170000
       WORKING-STORAGE SECTION.                                         00380000
       01 INFILE-FILE-STATUS      PIC X(2).                             00400000
       01 IN-EOF                  PIC 9 VALUE 0.                        00410000
       01 WS-VARIABLES.                                                 00400000
          05 WS-INFILE-CNTR-EDT   PIC ZZZ,ZZZ,ZZ9.                      00400000
          05 WS-INSERT-CNTR-EDT   PIC ZZZ,ZZZ,ZZ9.                      00400000
       01 WS-COUNTERS.                                                  00400000
          05 WS-CUSTMAST-CNTR     PIC 9(9) VALUE ZEROES.                00400000
          05 WS-TRANFILE-CNTR     PIC 9(9) VALUE ZEROES.                00400000
          05 WS-INFILE-CNTR       PIC 9(9) VALUE ZEROES.                00400000
          05 WS-INSERT-CNTR       PIC 9(9) VALUE ZEROES.                00400000
          05 WS-INSERT-CNTR1      PIC 9(9) VALUE ZEROES.                00400000
       01 WS-SQL-CODE             PIC S9(9).                            00180001
          88 SQL-CODE-SUCCESSFUL           VALUE ZEROES.                00190001
          88 SQL-CODE-ROW-NOT-FOUND        VALUE +100.                  00200001

         COPY INREC.                                                    00160000

         EXEC SQL INCLUDE CLIENT END-EXEC.                              00400000
         EXEC SQL INCLUDE SQLCA END-EXEC.                               00160000

       PROCEDURE DIVISION.                                              00430000
                                                                        00170000
           PERFORM START-PROCESS                                        00430000

           PERFORM MAIN-PROCESS                                         00430000
             UNTIL IN-EOF = 1                                           00430000

           PERFORM END-PROCESS                                          00430000

           CONTINUE.                                                    00430000

       START-PROCESS.                                                   00440013

           PERFORM FILE-OPEN-PARA                                       00440313
                                                                        00440413
           INITIALIZE WS-VARIABLES                                      00440113
                      INPUT-FIL                                         00440113
                                                                        00440213
           PERFORM MAIN-FILE-READ-PARA                                  00440515

           MOVE 0  TO WS-INFILE-CNTR                                    00441115
                                                                        00441115
           PERFORM MAIN-FILE-READ-PARA                                  00440515
                                                                        00440515
           CONTINUE.                                                    00441215

       MAIN-PROCESS.                                                    00441315
                                                                        00441415
           PERFORM POPULATE-TABLE                                       00443115
                                                                        00443115
           PERFORM INSERT-TABLE                                         00443115
                                                                        00443115
           PERFORM MAIN-FILE-READ-PARA                                  00443715
                                                                        00443715
           EXEC SQL                                                     00443715
             COMMIT                                                     00443715
           END-EXEC                                                     00443715
                                                                        00443715
           CONTINUE.                                                    00443715

       END-PROCESS.                                                     00443715

           MOVE WS-INFILE-CNTR  TO WS-INFILE-CNTR-EDT                   00443715
           MOVE WS-INSERT-CNTR  TO WS-INSERT-CNTR-EDT                   00443715

           DISPLAY '***************************************'            00443715
           DISPLAY '* PROGRAM NAME  :    MASTRAND         *'            00443715
           DISPLAY '* INFILE        : ' WS-INFILE-CNTR-EDT              00443715
                   '         *'                                         00443715
           DISPLAY '* CLIENT_JOIN2  : ' WS-INSERT-CNTR-EDT              00443715
                   '         *'                                         00443715
           DISPLAY '***************************************'            00443715

           CLOSE INFILE                                                 00443715

           STOP RUN.                                                    00443715

       POPULATE-TABLE.                                                  00443715

           INITIALIZE CLIENT                                            00443715

           MOVE IN1-CUST-ID          TO CLI-CUSTOMER-ID-TEXT            00443715
           MOVE 10                   TO CLI-CUSTOMER-ID-LEN             00443715
           MOVE IN1-CONT-ID          TO CLI-CONT-ID                     00443715
           MOVE IN1-HIGHEST-EDU      TO CLI-HIGHEST-EDU-TEXT            00443715
           MOVE 1                    TO CLI-HIGHEST-EDU-LEN             00443715
           MOVE IN1-ACTIVITY-LEVEL   TO CLI-ACTIVITY-LEVEL-TEXT         00443715
           MOVE 1                    TO CLI-ACTIVITY-LEVEL-LEN          00443715
           MOVE IN1-CHURN            TO CLI-CHURN-TEXT                  00443715
           MOVE 1                    TO CLI-CHURN-LEN                   00443715
           MOVE FUNCTION NUMVAL(IN1-AGE-YEARS)                          00443715
                                     TO CLI-AGE-YEARS                   00443715
           MOVE FUNCTION NUMVAL(IN1-ANNUAL-INVEST)                      00443715
                                     TO CLI-ANNUAL-INVESTMENT-REV       00443715
           MOVE FUNCTION NUMVAL(IN1-ANNUAL-INCOME)                      00443715
                                     TO CLI-ANNUAL-INCOME               00443715
           MOVE FUNCTION NUMVAL(IN1-TOTAL-TXN-AMOUNT)                   00443715
                                     TO CLI-TOTAL-TXN-AMOUNT            00443715

           IF IN1-GENDER = 'MALE'                                       00443715
           THEN                                                         00443715
             MOVE 1                  TO CLI-GENDER-TEXT                 00443715
           ELSE                                                         00443715
             MOVE 0                  TO CLI-GENDER-TEXT                 00443715
           END-IF                                                       00443715
           MOVE 1                    TO CLI-GENDER-LEN                  00443715

           MOVE FUNCTION NUMVAL(IN1-AVG-TXN-MOUNT)                      00443715
                                     TO CLI-AVG-TXN-AMOUNT              00443715
           MOVE IN1-TOTAL-TXNS       TO CLI-TOTAL-TXNS                  00443715
           MOVE FUNCTION NUMVAL(IN1-DAILY-TXNS)                         00443715
                                     TO CLI-AVG-DAILY-TXNS              00443715

           CONTINUE.                                                    00443715

       INSERT-TABLE.                                                    00443715

           EXEC SQL                                                     00443715
             INSERT INTO CLIENT_JOIN2                                   00443715
               (CUSTOMER_ID                                             00443715
               ,GENDER                                                  00443715
               ,AGE_YEARS                                               00443715
               ,HIGHEST_EDU                                             00443715
               ,ANNUAL_INVESTMENT_REV                                   00443715
               ,ANNUAL_INCOME                                           00443715
               ,TOTAL_TXNS                                              00443715
               ,AVG_DAILY_TXNS                                          00443715
               ,TOTAL_TXN_AMOUNT                                        00443715
               ,AVG_TXN_AMOUNT                                          00443715
               ,ACTIVITY_LEVEL                                          00443715
               ,CHURN                                                   00443715
               ,CONT_ID)                                                00443715
             VALUES                                                     00443715
              (:CLI-CUSTOMER-ID                                         00443715
              ,:CLI-GENDER                                              00443715
              ,:CLI-AGE-YEARS                                           00443715
              ,:CLI-HIGHEST-EDU                                         00443715
              ,:CLI-ANNUAL-INVESTMENT-REV                               00443715
              ,:CLI-ANNUAL-INCOME                                       00443715
              ,:CLI-TOTAL-TXNS                                          00443715
              ,:CLI-AVG-DAILY-TXNS                                      00443715
              ,:CLI-TOTAL-TXN-AMOUNT                                    00443715
              ,:CLI-AVG-TXN-AMOUNT                                      00443715
              ,:CLI-ACTIVITY-LEVEL                                      00443715
              ,:CLI-CHURN                                               00443715
              ,:CLI-CONT-ID)                                            00443715
           END-EXEC                                                     00443715

           MOVE SQLCODE TO WS-SQL-CODE                                  00540001
           EVALUATE TRUE                                                00550001
             WHEN SQL-CODE-SUCCESSFUL                                   00560001
               ADD 1 TO WS-INSERT-CNTR                                  00620001
                        WS-INSERT-CNTR1                                 00443715
               IF WS-INSERT-CNTR1 = 100                                 00443715
                  MOVE 0 TO WS-INSERT-CNTR1                             00443715
                  EXEC SQL                                              00443715
                   COMMIT                                               00443715
                  END-EXEC                                              00443715
               END-IF                                                   00443715
             WHEN OTHER                                                 00610001
               DISPLAY " ERROR INSERTING RECORD "                       00620001
               DISPLAY " SQL ERROR CODE " SQLCODE                       00630001
               DISPLAY " SQLSTATE " SQLSTATE                            00630001
               DISPLAY " SQLERRM " SQLERRM                              00630001

               PERFORM END-PROCESS                                      00630001
           END-EVALUATE                                                 00640001

           CONTINUE.                                                    00443715
                                                                        00480000
       FILE-OPEN-PARA.                                                  00490000
           OPEN INPUT  INFILE                                           00500000

           IF INFILE-FILE-STATUS NOT = '00'                             00443715
           THEN                                                         00443715
             DISPLAY 'OPEN INPUT FILE-STATUS = '  INFILE-FILE-STATUS    00443715

             PERFORM END-PROCESS                                        00443715
           END-IF                                                       00443715

           CONTINUE.                                                    00443715

       MAIN-FILE-READ-PARA.                                             00540000

           READ INFILE                                                  00550000
             AT END                                                     00560000
                MOVE 1 TO IN-EOF                                        00560000
           END-READ                                                     00570000

           IF  INFILE-FILE-STATUS NOT = '00'                            00443715
           THEN                                                         00443715
             IF  INFILE-FILE-STATUS NOT = '10'                          00443715
               DISPLAY 'MAIN FILE-STATUS = '  INFILE-FILE-STATUS        00443715
                                                                        00443715
               PERFORM END-PROCESS                                      00443715
             END-IF                                                     00443715
           ELSE                                                         00443715
             ADD 1 TO WS-INFILE-CNTR                                    00443715

             UNSTRING INFILE-REC DELIMITED BY ','                       00443715
                 INTO IN1-CUST-ID                                       00443715
                     ,IN1-GENDER                                        00443715
                     ,IN1-AGE-YEARS                                     00443715
                     ,IN1-HIGHEST-EDU                                   00443715
                     ,IN1-ANNUAL-INVEST                                 00443715
                     ,IN1-ANNUAL-INCOME                                 00443715
                     ,IN1-ACTIVITY-LEVEL                                00443715
                     ,IN1-CHURN                                         00443715
                     ,IN1-CONT-ID                                       00443715
                     ,IN1-TOTAL-TXNS                                    00443715
                     ,IN1-TOTAL-TXN-AMOUNT                              00443715
                     ,IN1-AVG-TXN-MOUNT                                 00443715
                     ,IN1-DAILY-TXNS                                    00443715
             END-UNSTRING                                               00443715
           END-IF                                                       00443715

           CONTINUE.                                                    00443715

       END PROGRAM  MASTRAND.                                           00443715
