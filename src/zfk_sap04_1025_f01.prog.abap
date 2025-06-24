*&---------------------------------------------------------------------*
*&  Include           ZFK_SAP04_1025_F01
*&---------------------------------------------------------------------*


FORM sum .

gv_result = p_1 + p_2.
    MESSAGE |Result: { gv_result }| TYPE 'I'.

ENDFORM.

FORM sub .

    gv_result = p_1 - p_2.
    MESSAGE |Result: { gv_result }| TYPE 'I'.

ENDFORM.

FORM multiply .

    gv_result = p_1 * p_2.
    MESSAGE |Result: { gv_result }| TYPE 'I'.

ENDFORM.

FORM divide .

    IF p_2 NE 0.
      gv_result = p_1 / p_2.
      MESSAGE |Result: { gv_result }| TYPE 'I'.
    ELSEIF p_2 = 0.
      MESSAGE 'Division by zero is not allowed.' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

ENDFORM.
