CREATE TABLE SPPAYTB1
    (ACAUREQ_HDR_CREDTT VARCHAR(40) WITH DEFAULT NULL,
    ACAUREQ_AUREQ_ENV_A_ID_ID        VARCHAR(40)
      WITH DEFAULT NULL,
    ACAUREQ_AUREQ_ENV_M_ID_ID        VARCHAR(40)
      WITH DEFAULT NULL,
    ACAUREQ_AUREQ_ENV_M_CMONNM       VARCHAR(40)
      WITH DEFAULT NULL,
    ACAUREQ_AUREQ_ENV_CPL_PAN        VARCHAR(40)
      WITH DEFAULT NULL,
    ACAUREQ_AUREQ_ENV_C_CARDBRND     VARCHAR(40)
      WITH DEFAULT NULL,
    ACAUREQ_AUREQ_TX_MRCHNTCTGYCD    VARCHAR(40)
      WITH DEFAULT NULL,
    ACAUREQ_AUREQ_TX_DT_TTLAMT       VARCHAR(40)
      WITH DEFAULT NULL,
    CONT_ID              DECIMAL(10, 0) WITH DEFAULT NULL,
    MDM_POSTAL_CODE_ID   VARCHAR(40)
      WITH DEFAULT NULL,
    AGE                  INTEGER WITH DEFAULT NULL,
    AUTHORRESULT_RSPNT   VARCHAR(40)
      WITH DEFAULT NULL,
    FRAUD_VER_RESULT     VARCHAR(40)
      WITH DEFAULT NULL)
  IN USERSPACE1  ;
