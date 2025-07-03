*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1035
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1035.

DATA: gs_str    TYPE sbook,
      gv_fldate TYPE char10.

SELECT SINGLE * FROM sbook INTO gs_str WHERE connid = 0106.


CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
  EXPORTING
    date_internal            = gs_str-fldate
  IMPORTING
    date_external            = gv_fldate
  EXCEPTIONS
    date_internal_is_invalid = 1
    OTHERS                   = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

WRITE | The flight time of flight { gs_str-connid } is { gv_fldate }. |.
