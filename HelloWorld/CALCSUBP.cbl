       ID DIVISION.                                                     00010005
       PROGRAM-ID. CALCSUBP.                                            00020005
       AUTHOR. TESTING.                                                 00040005
       DATA DIVISION.                                                   00050005
       WORKING-STORAGE SECTION.                                         00060005
       01 WS-VARIABLES.                                                 00480005
          05 WS-START-POS          PIC 9(2).                            00390000
                                                                        00460005
       LINKAGE SECTION.                                                 00470005
       01 LNK-VARIABLES.                                                00480005
          05 LNK-INPUT-DATA.                                            00400000
             10 LNK-FIELD          PIC X(20).                           00400000
             10 LNK-DEC-PLACE      PIC 9(2).                            00400000
          05 LNK-OUTPUT-DATA.                                           00400000
             10 LNK-WHOLE-CNTR     PIC 9(2).                            00400000
             10 LNK-DEC-CNTR       PIC 9(2).                            00400000
                                                                        00460005
       PROCEDURE DIVISION USING LNK-VARIABLES.                          00510005
       1000-MAINLINE.                                                   00530005
                                                                        00460005
           INITIALIZE WS-VARIABLES                                      00460005
                                                                        00460005
           INSPECT LNK-FIELD TALLYING LNK-WHOLE-CNTR                    00443715
               FOR LEADING SPACE                                        00443715
           INSPECT FUNCTION REVERSE (LNK-FIELD)                         00443715
           TALLYING WS-START-POS                                        00443715
               FOR LEADING SPACES                                       00443715
           INSPECT FUNCTION REVERSE (LNK-FIELD                          00443715
                        (1:LENGTH OF LNK-FIELD - WS-START-POS))         00443715
           TALLYING LNK-DEC-CNTR                                        00443715
               FOR LEADING ZEROES                                       00443715
                                                                        00460005
           IF LNK-DEC-PLACE > 0                                         00443715
           THEN                                                         00443715
             IF LNK-DEC-CNTR = LNK-DEC-PLACE                            00443715
             THEN                                                       00443715
      *         SUBTRACT 1 FROM LNK-DEC-CNTR                             00443715
               ADD 1 TO LNK-DEC-CNTR                                    00443715
             END-IF                                                     00443715
           END-IF.                                                      00443715
                                                                        00460005
           GOBACK.                                                      00620005
