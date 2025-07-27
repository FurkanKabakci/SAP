FUNCTION ZFK_SAP04_F07.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_ANSWER) TYPE  CHAR1
*"     REFERENCE(EV_COLOR_NUMBER) TYPE  CHAR1
*"     REFERENCE(EV_INTENSIFIED) TYPE  CHAR1
*"     REFERENCE(EV_INVERSE) TYPE  CHAR1
*"----------------------------------------------------------------------
CALL SELECTION-SCREEN 1040 STARTING AT 5 5 ENDING AT 165 8.

IF sy-subrc IS INITIAL.
  EV_ANSWER       = sy-subrc.
  EV_COLOR_NUMBER = p_c_nu.
  EV_INTENSIFIED  = p_int.
  EV_INVERSE      = p_inv.
ELSE.
  EV_ANSWER = sy-subrc.

ENDIF.
ENDFUNCTION.
