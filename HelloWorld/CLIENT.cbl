      ******************************************************************
      * DCLGEN TABLE(DB2INST1.CLIENT_JOIN1)                            *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(CLI-)                                             *
      *        STRUCTURE(CLIENT)                                       *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2INST1.CLIENT_JOIN2 TABLE                 
           ( CUSTOMER_ID                    VARCHAR(20) NOT NULL,       
             GENDER                         VARCHAR(1),                 
             AGE_YEARS                      DECIMAL(12, 2),              
             HIGHEST_EDU                    VARCHAR(1),                 
             ANNUAL_INVESTMENT_REV          DECIMAL(18, 2),             
             ANNUAL_INCOME                  DECIMAL(18, 2),             
             TOTAL_TXNS                     DECIMAL(12,0),              
             AVG_DAILY_TXNS                 DECIMAL(8, 2),              
             TOTAL_TXN_AMOUNT               DECIMAL(16, 2),             
             AVG_TXN_AMOUNT                 DECIMAL(16, 2),             
             ACTIVITY_LEVEL                 VARCHAR(1),                 
             CHURN                          VARCHAR(1),                 
             CONT_ID                        CHAR(10)                    
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2INST1.CLIENT_JOIN1              *
      ******************************************************************
       01  CLIENT.                                                      
           10 CLI-CUSTOMER-ID.                                          
      *                       CUSTOMER_ID LENGTH                        
              49 CLI-CUSTOMER-ID-LEN                                    
                 PIC S9(4) USAGE COMP.                                  
      *                       CUSTOMER_ID                               
              49 CLI-CUSTOMER-ID-TEXT                                   
                 PIC X(20).                                             
           10 CLI-GENDER.                                               
      *                       GENDER LENGTH                             
              49 CLI-GENDER-LEN    PIC S9(4) USAGE COMP.                
      *                       GENDER                                    
              49 CLI-GENDER-TEXT   PIC X(1).                            
      *                       AGE_YEARS                                 
           10 CLI-AGE-YEARS        PIC S9(10)V9(2) USAGE COMP-3.         
           10 CLI-HIGHEST-EDU.                                          
      *                       HIGHEST_EDU LENGTH                        
              49 CLI-HIGHEST-EDU-LEN                                    
                 PIC S9(4) USAGE COMP.                                  
      *                       HIGHEST_EDU                               
              49 CLI-HIGHEST-EDU-TEXT                                   
                 PIC X(1).                                              
      *                       ANNUAL_INVESTMENT_REV                     
           10 CLI-ANNUAL-INVESTMENT-REV                                 
              PIC S9(16)V9(2) USAGE COMP-3.                             
      *                       ANNUAL_INCOME                             
           10 CLI-ANNUAL-INCOME    PIC S9(16)V9(2) USAGE COMP-3.        
      *                       TOTAL_TXNS                                
           10 CLI-TOTAL-TXNS       PIC S9(12) USAGE COMP.                
      *                       AVG_DAILY_TXNS                            
           10 CLI-AVG-DAILY-TXNS   PIC S9(6)V9(2) USAGE COMP-3.         
      *                       TOTAL_TXN_AMOUNT                          
           10 CLI-TOTAL-TXN-AMOUNT                                      
              PIC S9(14)V9(2) USAGE COMP-3.                              
      *                       AVG_TXN_AMOUNT                            
           10 CLI-AVG-TXN-AMOUNT   PIC S9(14)V9(2) USAGE COMP-3.         
           10 CLI-ACTIVITY-LEVEL.                                       
      *                       ACTIVITY_LEVEL LENGTH                     
              49 CLI-ACTIVITY-LEVEL-LEN                                 
                 PIC S9(4) USAGE COMP.                                  
      *                       ACTIVITY_LEVEL                            
              49 CLI-ACTIVITY-LEVEL-TEXT                                
                 PIC X(1).                                              
           10 CLI-CHURN.                                                
      *                       CHURN LENGTH                              
              49 CLI-CHURN-LEN     PIC S9(4) USAGE COMP.                
      *                       CHURN                                     
              49 CLI-CHURN-TEXT    PIC X(1).                            
      *                       CONT_ID                                   
           10 CLI-CONT-ID          PIC X(10).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
