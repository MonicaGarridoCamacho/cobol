  CREATE TABLE CLIENT_JOIN2 (            
    CUSTOMER_ID VARCHAR(20) NOT NULL,    
    GENDER VARCHAR(1),                   
    AGE_YEARS DECIMAL(12,2),              
    HIGHEST_EDU VARCHAR(1),              
    ANNUAL_INVESTMENT_REV DECIMAL(18,2), 
    ANNUAL_INCOME DECIMAL(18,2),         
    TOTAL_TXNS DECIMAL(12,0),                 
    AVG_DAILY_TXNS DECIMAL(8,2),         
    TOTAL_TXN_AMOUNT DECIMAL(16,2),      
    AVG_TXN_AMOUNT DECIMAL(16,2),        
    ACTIVITY_LEVEL VARCHAR(1),           
    CHURN VARCHAR(1),                    
    CONT_ID CHAR(10),               
   PRIMARY KEY(CUSTOMER_ID))
   IN USERSPACE1;
