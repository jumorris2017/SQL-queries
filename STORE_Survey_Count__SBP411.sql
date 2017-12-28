/* Pulls the number of surveys associated with a store */
/* For Cambridge requests */

SELECT ce.STORE_NUM, ce.GUID_USER_ID, TRUNC(ce.TRANS_DTM), COUNT(*) FROM APPDWH.AFT_CV_SRVY_RSPNS ce
    WHERE ce.STORE_NUM IN (48894)
    GROUP BY ce.STORE_NUM, ce.GUID_USER_ID, TRUNC(ce.TRANS_DTM)
