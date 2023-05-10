#!/bin/sh
#Set the database to be used to environment variable
export DB2DBDFT=testdb
#Establish the connection to the database to be used
db2 connect to testdb
#Unload the transactions from SPPAYTB1 db2-table
db2 "EXPORT TO /home/db2inst1/Tab.txt OF DEL MODIFIED BY NOCHARDEL SELECT substr(ACAUREQ_HDR_CREDTT,1,40) || substr(ACAUREQ_AUREQ_ENV_A_ID_ID,1,40) || substr(ACAUREQ_AUREQ_ENV_M_ID_ID,1,40) || substr(ACAUREQ_AUREQ_ENV_M_CMONNM,1,40) || substr(ACAUREQ_AUREQ_ENV_CPL_PAN,1,40) || substr(ACAUREQ_AUREQ_ENV_C_CARDBRND,1,40) || substr(ACAUREQ_AUREQ_TX_MRCHNTCTGYCD,1,40) || substr(ACAUREQ_AUREQ_TX_DT_TTLAMT,1,40) || char('0') || cast(CONT_ID as decimal(10)) || substr(MDM_POSTAL_CODE_ID,1,40) || substr(AGE,1,2) || substr(AUTHORRESULT_RSPNT,1,40) || substr(FRAUD_VER_RESULT,1,40) FROM db2inst1.SPPAYTB1 ORDER BY CONT_ID ASC"
#Set the input and output files for MASTRANA
#Customer file
export CUSTMAST=RSD-/home/db2inst1/CUSTMAST
#Unloaded transaction file
export TRANFILE=RSD-/home/db2inst1/Tab.txt
#Report file
export OUTFILE=RSD-/home/db2inst1/OUTPUT.txt
#Execute MASTRANA to produce a summary report of transactions per customer
"/home/db2inst1/MASTRANA"
#Clean the data from CLIENT_JOIN2 table for the new results to be stored
db2 DELETE FROM db2inst1.CLIENT_JOIN2
#Set the input file for MASTRANB
export INFILE=RSD-/home/db2inst1/OUTPUT.txt
#Execute MASTRANB to save the summary report of transactions per customer in CLIENT_JOIN2 table
"/home/db2inst1/MASTRANB"
#Check sample inserted records in CLIENT_JOIN2 db2 table
db2 "SELECT * FROM db2inst1.CLIENT_JOIN2 FETCH FIRST 2 ROW ONLY"
db2 terminate

