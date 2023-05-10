       ID DIVISION.                                                     00010000
       PROGRAM-ID. MASTRANC.                                            00020000
       ENVIRONMENT DIVISION.                                            00030000
       INPUT-OUTPUT SECTION.                                            00040000
       FILE-CONTROL.                                                    00050000
            SELECT CUSTMAST ASSIGN TO CUSTMAST                          00110000
            ORGANIZATION IS SEQUENTIAL                                  00120000
            ACCESS MODE IS SEQUENTIAL                                   00120000
            FILE STATUS IS CUSTFILE-FILE-STATUS.                        00000000
            SELECT TRANFILE ASSIGN TO TRANFILE                          00110000
            ORGANIZATION IS SEQUENTIAL                                  00120000
            FILE STATUS IS TRANFILE-FILE-STATUS.                        00130000
            SELECT OUTFILE ASSIGN TO OUTFILE                            00110000
            ORGANIZATION IS SEQUENTIAL                                  00120000
            FILE STATUS IS OUTFILE-FILE-STATUS.                         00130000
       DATA DIVISION.                                                   00140000
       FILE SECTION.                                                    00150000
       FD CUSTMAST.                                                     00160000
         COPY CUSTREC.                                                  00160000

       FD TRANFILE.                                                     00230000
       01 TRANS-REC.                                                    00240011
          10 TXN-HDR-CREDTT                                             00170000
                PIC X(40).                                              00170000
          10 TXN-AUREQ-ENV-A-ID-ID                                      00170000
                PIC X(40).                                              00170000
          10 TXN-AUREQ-ENV-M-ID-ID                                      00170000
                PIC X(40).                                              00170000
          10 TXN-AUREQ-ENV-M-CMONNM                                     00170000
                PIC X(40).                                              00170000
          10 TXN-AUREQ-ENV-CPL-PAN                                      00170000
                PIC X(40).                                              00170000
          10 TXN-AUREQ-ENV-C-CARDBRND                                   00170000
                PIC X(40).                                              00170000
          10 TXN-AUREQ-TX-MRCHNTCTGYCD                                  00170000
                PIC X(40).                                              00170000
          10 TXN-AUREQ-TX-DT-TTLAMT                                     00170000
                PIC X(40).                                              00170000
          10 TRAN-CONT-ID         PIC 9(10).                            00170000
          10 MDM-POSTAL-CODE-ID                                         00170000
                PIC X(40).                                              00170000
          10 AGE                  PIC 9(2).                             00170000
          10 AUTHORRESULT-RSPNT                                         00170000
                PIC X(40).                                              00170000
          10 FRAUD-VER-RESULT                                           00170000
                PIC X(40).                                              00170000

       FD OUTFILE.                                                      00230000
       01 OUTPUT-REC              PIC X(200).                           00170000
                                                                        00170000
       WORKING-STORAGE SECTION.                                         00380000
       01 CUSTFILE-FILE-STATUS PIC X(2).                                00390000
       01 TRANFILE-FILE-STATUS PIC X(2).                                00400000
       01 OUTFILE-FILE-STATUS  PIC X(2).                                00400000
       01 MAS-EOF PIC 9 VALUE 0.                                        00410000
       01 TRAN-EOF PIC 9 VALUE 0.                                       00410000
       01 WS-TRANSCODE PIC 9(2).                                        00420010
       01 WS-TRAN-CONT-ID         PIC 9(10).                            00420010

         COPY OUTREC.                                                   00160000

       01 WS-CONSTANTS.                                                 00400000
          05 WS-SEPARATOR         PIC X(1) VALUE ','.                   00400000
          05 WS-HEADER.                                                 00400000
          10 WS-HEADER-1          PIC X(112) VALUE                      00400000
            'POLICY_NUMBER,GENDER,AGE,POLICY_TYPE_CODE,ASSESSED_VALUE,AP00400000
      -     'PRAISAL,OCCUPANCY,INSURANCE_SCORE,ACCOUNT_NUMBER,TOTA'.    00400000
          10 WS-HEADER-2          PIC X(88) VALUE                       00400000
            'L_PAYMENTS,TOTAL_PAYMENTS_AMOUNT,AVERAGE_PAYMENTS_AMOUNT,DE00400000
      -     'DUCTABLE,LIMITS_OF_INSURANCE,'.                            00400000
       01 WS-VARIABLES.                                                 00400000
          05 WS-TIMESTAMP         PIC X(23).                            00400000
          05 WS-HH                PIC 9(02).                            00400000
          05 WS-MM                PIC 9(02).                            00400000
          05 WS-TIMESTAMP2        PIC X(26).                            00400000
          05 WS-MIN-DATE          PIC X(10).                            00400000
          05 WS-MAX-DATE          PIC X(10).                            00400000
      *   05 WS-MIN-DATE          PIC 9(08).                            00400000
      *   05 WS-MAX-DATE          PIC 9(08).                            00400000
          05 WS-DAYS              PIC 9(18).                            00400000
          05 WS-TOTAL-TXN-AMOUNT  PIC 9(16)V99.                         00400000
          05 WS-AVG-TXN-MOUNT     PIC 9(12)V9(6).                       00400000
          05 WS-TOTAL-TXNS        PIC 9(18) VALUE ZEROES.               00400000
          05 WS-CUSTMAST-CNTR-EDT PIC ZZZ,ZZZ,ZZ9.                      00400000
          05 WS-TRANFILE-CNTR-EDT PIC ZZZ,ZZZ,ZZ9.                      00400000
          05 WS-OUTFILE-CNTR-EDT  PIC ZZZ,ZZZ,ZZ9.                      00400000
       01 WS-COUNTERS.                                                  00400000
          05 WC-GENDER-CNTR       PIC 9(2) VALUE ZEROES.                00400000
          05 WS-TXN-WHOLE-CNTR    PIC 9(2) VALUE ZEROES.                00400000
          05 WS-AGE-WHOLE-CNTR    PIC 9(2) VALUE ZEROES.                00400000
          05 WS-AGE-DEC-CNTR      PIC 9(2) VALUE ZEROES.                00400000
          05 WS-INV-WHOLE-CNTR    PIC 9(2) VALUE ZEROES.                00400000
          05 WS-INV-DEC-CNTR      PIC 9(2) VALUE ZEROES.                00400000
          05 WS-INC-WHOLE-CNTR    PIC 9(2) VALUE ZEROES.                00400000
          05 WS-INC-DEC-CNTR      PIC 9(2) VALUE ZEROES.                00400000
          05 WS-TOT-WHOLE-CNTR    PIC 9(2) VALUE ZEROES.                00400000
          05 WS-TOT-DEC-CNTR      PIC 9(2) VALUE ZEROES.                00400000
          05 WS-AVG-WHOLE-CNTR    PIC 9(2) VALUE ZEROES.                00400000
          05 WS-AVG-DEC-CNTR      PIC 9(2) VALUE ZEROES.                00400000
          05 WS-DAI-WHOLE-CNTR    PIC 9(2) VALUE ZEROES.                00400000
          05 WS-DAI-DEC-CNTR      PIC 9(2) VALUE ZEROES.                00400000
       01 WS-COUNTERS-1.                                                00400000
          05 WS-CUSTMAST-CNTR     PIC 9(9) VALUE ZEROES.                00400000
          05 WS-TRANFILE-CNTR     PIC 9(9) VALUE ZEROES.                00400000
          05 WS-OUTFILE-CNTR      PIC 9(9) VALUE ZEROES.                00400000
       01 WS-LINKAGE-DATA.                                              00480005
          05 LNK-INPUT.                                                 00400000
             10 LNK-FIELD          PIC X(20).                           00400000
             10 LNK-DEC-PLACE      PIC 9(2).                            00400000
          05 LNK-OUTPUT.                                                00400000
             10 LNK-WHOLE-CNTR     PIC 9(2).                            00400000
             10 LNK-DEC-CNTR       PIC 9(2).                            00400000

       PROCEDURE DIVISION.                                              00430000
                                                                        00170000
           PERFORM START-PROCESS                                        00430000

           PERFORM MAIN-PROCESS                                         00430000

           PERFORM END-PROCESS                                          00430000

           CONTINUE.                                                    00430000

       START-PROCESS.                                                   00440013

           DISPLAY 'START PROCESS'                                      00440313

           PERFORM FILE-OPEN-PARA                                       00440313
                                                                        00440413
           INITIALIZE WS-VARIABLES                                      00440113
                      WS-TRAN-CONT-ID                                   00440113
                      CUST-REC                                          00440113
                      TRANS-REC                                         00440113
                      OUTPUT-FIL                                        00440113
                                                                        00440213
           WRITE OUTPUT-REC FROM WS-HEADER                              00440213
                                                                        00440213
           PERFORM MAIN-FILE-READ-PARA                                  00440515
                                                                        00440713
           PERFORM TRANSFILE-READ-PARA WITH TEST BEFORE                 00440815
             UNTIL MAIN-CONT-ID = WS-TRAN-CONT-ID                       00440915
                OR MAIN-CONT-ID < WS-TRAN-CONT-ID                       00441015
                OR TRAN-EOF = 1                                         00441015
                                                                        00441115
           CONTINUE.                                                    00441215

       MAIN-PROCESS.                                                    00441315
                                                                        00442013
           PERFORM WITH TEST BEFORE                                     00442013
             UNTIL MAS-EOF = 1                                          00443013
                OR TRAN-EOF = 1                                         00443013
                                                                        00443115
             IF MAIN-CONT-ID = TRAN-CONT-ID                             00443215
             THEN                                                       00443315

               ADD 1 TO WS-TOTAL-TXNS                                   00443615
                                                                        00443715
               COMPUTE WS-TOTAL-TXN-AMOUNT = FUNCTION NUMVAL            00443715
                 (TXN-AUREQ-TX-DT-TTLAMT)  + WS-TOTAL-TXN-AMOUNT        00443715

               PERFORM TRANSFILE-READ-PARA                              00443715
             ELSE                                                       00443715
               IF WS-TOTAL-TXNS > 0                                     00443715
               THEN                                                     00443715
                 PERFORM WRITE-OUTPUT-FILE                              00443715
               END-IF                                                   00443715

               INITIALIZE WS-VARIABLES                                  00443715
                          OUTPUT-FIL                                    00443715

               PERFORM MAIN-FILE-READ-PARA                              00443715

               PERFORM TRANSFILE-READ-PARA WITH TEST BEFORE             00443715
                 UNTIL MAIN-CONT-ID = WS-TRAN-CONT-ID                   00443715
                    OR MAIN-CONT-ID < WS-TRAN-CONT-ID                   00443715
                    OR TRAN-EOF = 1                                     00443715
             END-IF                                                     00443715
                                                                        00444013
           END-PERFORM                                                  00445014

           IF TRAN-EOF = 1                                              00443715
           AND MAS-EOF = 0                                              00443715
           AND WS-TOTAL-TXNS > 0                                        00443715
           THEN                                                         00443715
             PERFORM WRITE-OUTPUT-FILE                                  00443715
           END-IF                                                       00443715
                                                                        00446014
           CONTINUE.                                                    00443715

       END-PROCESS.                                                     00443715

           MOVE WS-CUSTMAST-CNTR TO WS-CUSTMAST-CNTR-EDT                00443715
           MOVE WS-TRANFILE-CNTR TO WS-TRANFILE-CNTR-EDT                00443715
           MOVE WS-OUTFILE-CNTR  TO WS-OUTFILE-CNTR-EDT                 00443715

           DISPLAY '***************************************'            00443715
           DISPLAY '* PROGRAM NAME  :    MASTRANC         *'            00443715
           DISPLAY '* CUSTMAST      : ' WS-CUSTMAST-CNTR-EDT            00443715
                   '         *'                                         00443715
           DISPLAY '* TRANFILE      : ' WS-TRANFILE-CNTR-EDT            00443715
                   '         *'                                         00443715
           DISPLAY '* OUTFILE       : ' WS-OUTFILE-CNTR-EDT             00443715
                   '         *'                                         00443715
           DISPLAY '***************************************'            00443715

           CLOSE CUSTMAST.                                              00443715
           CLOSE TRANFILE.                                              00443715
           CLOSE OUTFILE.                                               00443715

           STOP RUN.                                                    00443715

       WRITE-OUTPUT-FILE.                                               00443715

           INITIALIZE WS-COUNTERS                                       00443715
                      OUTPUT-REC                                        00443715 

           MOVE MAIN-CONT-ID        TO OUT-CUST-ID                      00443715
                                       OUT-CONT-ID                      00443715
           MOVE HIGHEST-EDU         TO OUT-HIGHEST-EDU                  00443715
           MOVE ACTIVITY-LEVEL      TO OUT-ACTIVITY-LEVEL               00443715
           MOVE CHURN               TO OUT-CHURN                        00443715
                                                                                
           MOVE AGE-YEARS           TO OUT-AGE-YEARS                    00443715
           INITIALIZE WS-LINKAGE-DATA                                   00443715
           MOVE OUT-AGE-YEARS       TO LNK-FIELD                        00443715
           MOVE 2                   TO LNK-DEC-PLACE                    00443715
           CALL 'CALCSUBP'       USING WS-LINKAGE-DATA                  00443715
           MOVE LNK-WHOLE-CNTR      TO WS-AGE-WHOLE-CNTR                00443715
           MOVE LNK-DEC-CNTR        TO WS-AGE-DEC-CNTR                  00443715
                                                                                
           MOVE ANNUAL-INVEST       TO OUT-ANNUAL-INVEST                00443715
           INITIALIZE WS-LINKAGE-DATA                                   00443715
           MOVE OUT-ANNUAL-INVEST   TO LNK-FIELD                        00443715
           MOVE 3                   TO LNK-DEC-PLACE                    00443715
           CALL 'CALCSUBP'       USING WS-LINKAGE-DATA                  00443715
           MOVE LNK-WHOLE-CNTR      TO WS-INV-WHOLE-CNTR                00443715
           MOVE LNK-DEC-CNTR        TO WS-INV-DEC-CNTR                  00443715
                                                                                
           MOVE ANNUAL-INCOME       TO OUT-ANNUAL-INCOME                00443715
           INITIALIZE WS-LINKAGE-DATA                                   00443715
           MOVE OUT-ANNUAL-INCOME   TO LNK-FIELD                        00443715
           MOVE 3                   TO LNK-DEC-PLACE                    00443715
           CALL 'CALCSUBP'       USING WS-LINKAGE-DATA                  00443715
           MOVE LNK-WHOLE-CNTR      TO WS-INC-WHOLE-CNTR                00443715
           MOVE LNK-DEC-CNTR        TO WS-INC-DEC-CNTR                  00443715
                                                                                
           MOVE WS-TOTAL-TXN-AMOUNT TO OUT-TOTAL-TXN-AMOUNT             00443715
           INITIALIZE WS-LINKAGE-DATA                                   00443715
           MOVE OUT-TOTAL-TXN-AMOUNT                                    00443715
                                    TO LNK-FIELD                        00443715
           MOVE 2                   TO LNK-DEC-PLACE                    00443715
           CALL 'CALCSUBP'       USING WS-LINKAGE-DATA                  00443715
           MOVE LNK-WHOLE-CNTR      TO WS-TOT-WHOLE-CNTR                00443715
           MOVE LNK-DEC-CNTR        TO WS-TOT-DEC-CNTR                  00443715
                                                                                
           IF GENDER = 1                                                00443715
           THEN                                                         00443715
             MOVE 'MALE'            TO OUT-GENDER                       00443715
             MOVE 4                 TO WC-GENDER-CNTR                   00443715
           ELSE                                                         00443715
             MOVE 'FEMALE'          TO OUT-GENDER                       00443715
             MOVE 6                 TO WC-GENDER-CNTR                   00443715
           END-IF                                                       00443715

           COMPUTE WS-AVG-TXN-MOUNT = WS-TOTAL-TXN-AMOUNT               00443715
                                    / WS-TOTAL-TXNS                     00443715

           MOVE WS-AVG-TXN-MOUNT    TO OUT-AVG-TXN-MOUNT                00443715
           INITIALIZE WS-LINKAGE-DATA                                   00443715
           MOVE OUT-AVG-TXN-MOUNT   TO LNK-FIELD                        00443715
           MOVE 6                   TO LNK-DEC-PLACE                    00443715
           CALL 'CALCSUBP'       USING WS-LINKAGE-DATA                  00443715
           MOVE LNK-WHOLE-CNTR      TO WS-AVG-WHOLE-CNTR                00443715
           MOVE LNK-DEC-CNTR        TO WS-AVG-DEC-CNTR                  00443715

           MOVE WS-TOTAL-TXNS       TO OUT-TOTAL-TXNS                   00443715
           INITIALIZE WS-LINKAGE-DATA                                   00443715
           MOVE OUT-TOTAL-TXNS      TO LNK-FIELD                        00443715
           CALL 'CALCSUBP'       USING WS-LINKAGE-DATA                  00443715
           MOVE LNK-WHOLE-CNTR      TO WS-TXN-WHOLE-CNTR                00443715

           COMPUTE OUT-DAILY-TXNS = WS-TOTAL-TXNS / 365                 00443715
           INITIALIZE WS-LINKAGE-DATA                                   00443715
           MOVE OUT-DAILY-TXNS      TO LNK-FIELD                        00443715
           MOVE 6                   TO LNK-DEC-PLACE                    00443715
           CALL 'CALCSUBP'       USING WS-LINKAGE-DATA                  00443715
           MOVE LNK-WHOLE-CNTR      TO WS-DAI-WHOLE-CNTR                00443715
           MOVE LNK-DEC-CNTR        TO WS-DAI-DEC-CNTR                  00443715
                                                                                
           STRING OUT-CUST-ID                                           00443715
             WS-SEPARATOR OUT-GENDER(1:WC-GENDER-CNTR)                  00443715
             WS-SEPARATOR                                               00443715
             OUT-AGE-YEARS(WS-AGE-WHOLE-CNTR + 1:                       00443715
               LENGTH OF OUT-AGE-YEARS - (WS-AGE-WHOLE-CNTR +           00443715
               WS-AGE-DEC-CNTR))                                        00443715
             WS-SEPARATOR OUT-HIGHEST-EDU                               00443715
             WS-SEPARATOR                                               00443715
             OUT-ANNUAL-INVEST(WS-INV-WHOLE-CNTR + 1:                   00443715
               LENGTH OF OUT-ANNUAL-INVEST - (WS-INV-WHOLE-CNTR +       00443715
               WS-INV-DEC-CNTR))                                        00443715
             WS-SEPARATOR                                               00443715
             OUT-ANNUAL-INCOME(WS-INC-WHOLE-CNTR + 1:                   00443715
               LENGTH OF OUT-ANNUAL-INCOME - (WS-INC-WHOLE-CNTR +       00443715
               WS-INC-DEC-CNTR))                                        00443715
             WS-SEPARATOR OUT-ACTIVITY-LEVEL                            00443715
             WS-SEPARATOR OUT-CHURN                                     00443715
             WS-SEPARATOR OUT-CONT-ID                                   00443715
             WS-SEPARATOR                                               00443715
             OUT-TOTAL-TXNS(WS-TXN-WHOLE-CNTR + 1:                      00443715
               LENGTH OF OUT-TOTAL-TXNS - WS-TXN-WHOLE-CNTR)            00443715
             WS-SEPARATOR                                               00443715
             OUT-TOTAL-TXN-AMOUNT(WS-TOT-WHOLE-CNTR + 1:                00443715
               LENGTH OF OUT-TOTAL-TXN-AMOUNT - (WS-TOT-WHOLE-CNTR +    00443715
               WS-TOT-DEC-CNTR))                                        00443715
             WS-SEPARATOR                                               00443715
             OUT-AVG-TXN-MOUNT(WS-AVG-WHOLE-CNTR + 1:                   00443715
               LENGTH OF OUT-AVG-TXN-MOUNT - (WS-AVG-WHOLE-CNTR +       00443715
               WS-AVG-DEC-CNTR))                                        00443715
             WS-SEPARATOR                                               00443715
             OUT-DAILY-TXNS(WS-DAI-WHOLE-CNTR + 1:                      00443715
               LENGTH OF OUT-DAILY-TXNS - (WS-DAI-WHOLE-CNTR +          00443715
               WS-DAI-DEC-CNTR))                                        00443715
             WS-SEPARATOR                                               00443715
             DELIMITED BY SIZE                                          00443715
             INTO OUTPUT-REC                                            00443715

           WRITE OUTPUT-REC                                             00443715

           ADD 1 TO WS-OUTFILE-CNTR                                     00443715

           CONTINUE.                                                    00443715
                                                                        00480000
       FILE-OPEN-PARA.                                                  00490000
           OPEN INPUT  CUSTMAST                                         00500000

           IF CUSTFILE-FILE-STATUS NOT = '00'                           00443715
           THEN                                                         00443715
             DISPLAY 'OPEN MAIN FILE-STATUS = '  CUSTFILE-FILE-STATUS   00443715

             PERFORM END-PROCESS                                        00443715
           END-IF                                                       00443715

           OPEN INPUT  TRANFILE                                         00510000

           IF TRANFILE-FILE-STATUS NOT = '00'                           00443715
           THEN                                                         00443715
             DISPLAY 'OPEN TRANS FILE-STATUS = '  TRANFILE-FILE-STATUS  00443715

             PERFORM END-PROCESS                                        00443715
           END-IF                                                       00443715

           OPEN OUTPUT OUTFILE                                          00510000

           IF OUTFILE-FILE-STATUS NOT = '00'                            00443715
           THEN                                                         00443715
             DISPLAY 'OPEN OUTPUT FILE-STATUS = '  OUTFILE-FILE-STATUS  00443715

             PERFORM END-PROCESS                                        00443715
           END-IF                                                       00443715

           DISPLAY 'OPENING FILES SUCCESSFUL '                          00443715

           CONTINUE.                                                    00443715

       MAIN-FILE-READ-PARA.                                             00540000

           READ CUSTMAST                                                00550000
             AT END                                                     00560000
                MOVE 1 TO MAS-EOF                                       00560000
           END-READ                                                     00570000

           IF CUSTFILE-FILE-STATUS NOT = '00'                           00443715
           THEN                                                         00443715
             IF CUSTFILE-FILE-STATUS NOT = '10'                         00443715
             THEN                                                       00443715
               DISPLAY 'MAIN FILE-STATUS = '  CUSTFILE-FILE-STATUS      00443715
                                                                        00443715
               PERFORM END-PROCESS                                      00443715
             END-IF                                                     00443715
           ELSE                                                         00443715
             ADD 1 TO WS-CUSTMAST-CNTR                                  00443715
           END-IF                                                       00443715

           CONTINUE.                                                    00443715

       TRANSFILE-READ-PARA.                                             00540000
           READ TRANFILE                                                00550000
             AT END                                                     00560000
                MOVE 1 TO TRAN-EOF                                      00560000
           END-READ                                                     00570000

           IF TRANFILE-FILE-STATUS NOT = '00'                           00443715
           THEN                                                         00443715
             IF TRANFILE-FILE-STATUS NOT = '10'                         00443715
             THEN                                                       00443715
               DISPLAY 'TRANS FILE-STATUS = '  TRANFILE-FILE-STATUS     00443715
                                                                        00443715
               PERFORM END-PROCESS                                      00443715
             END-IF                                                     00443715
           ELSE                                                         00443715
             ADD 1 TO WS-TRANFILE-CNTR                                  00443715

             MOVE TRAN-CONT-ID          TO WS-TRAN-CONT-ID              00443715
           END-IF                                                       00443715

           CONTINUE.                                                    00443715

       END PROGRAM  MASTRANC.                                           00443715
