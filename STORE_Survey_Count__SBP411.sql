/* Pulls the number of surveys associated with a store */
/* For Cambridge requests */

SELECT ce.STORE_NUM, ce.GUID_USER_ID, TRUNC(ce.TRANS_DTM), COUNT(*) FROM APPDWH.AFT_CV_SRVY_RSPNS ce
    WHERE ce.STORE_NUM IN (23113)
    --AND TRUNC(ce.TRANS_DTM) > '05-JAN-18'
    GROUP BY ce.STORE_NUM, ce.GUID_USER_ID, TRUNC(ce.TRANS_DTM)
    ORDER BY TRUNC(ce.TRANS_DTM) DESC
