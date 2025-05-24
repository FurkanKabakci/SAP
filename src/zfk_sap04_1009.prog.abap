*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1009
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1009.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001 NO INTERVALS.
PARAMETERS: p_1      TYPE i,
            p_2      TYPE i,
            p_symbol TYPE c LENGTH 1.
SELECTION-SCREEN END OF BLOCK a1.

DATA: gv_result TYPE p DECIMALS 2.


START-OF-SELECTION.

  IF p_symbol = '+'.
    gv_result = p_1 + p_2.
    MESSAGE |Result: { gv_result }| TYPE 'I'.

  ELSEIF p_symbol = '-'.
    gv_result = p_1 - p_2.
    MESSAGE |Result: { gv_result }| TYPE 'I'.

  ELSEIF p_symbol = '*'.
    gv_result = p_1 * p_2.
    MESSAGE |Result: { gv_result }| TYPE 'I'.

  ELSEIF p_symbol = '/'.
    IF p_2 NE 0.
      gv_result = p_1 / p_2.
      MESSAGE |Result: { gv_result }| TYPE 'I'.
    ELSEIF p_2 = 0.
      MESSAGE 'Division by zero is not allowed.' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ELSE.
    MESSAGE 'Invalid Operation' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.
