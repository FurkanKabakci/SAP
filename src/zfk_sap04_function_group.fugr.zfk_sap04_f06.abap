FUNCTION ZFK_SAP04_F06.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_ANSWER) TYPE  CHAR1
*"     REFERENCE(EV_COLOR) TYPE  LVC_EMPHSZ
*"----------------------------------------------------------------------

CALL SELECTION-SCREEN 1039 STARTING AT 5 5 ENDING AT 165 8.
IF sy-subrc is INITIAL.
  ev_color  = p_color.
  ev_answer = sy-subrc.
ELSE.
  ev_answer = sy-subrc.
ENDIF.

ENDFUNCTION.
