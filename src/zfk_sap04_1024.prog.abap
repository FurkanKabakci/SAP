*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1024
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFK_SAP04_1024.

*PERFORM executes the code defined within a FORM…ENDFORM block.
*It is used to avoid code repetition and to make the logic more modular.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001 NO INTERVALS.
PARAMETERS: p_1      TYPE i,
            p_2      TYPE i,
            p_symbol TYPE c LENGTH 1.
SELECTION-SCREEN END OF BLOCK a1.

DATA: gv_result TYPE p DECIMALS 2.


START-OF-SELECTION.


  IF p_symbol = '+'.

    PERFORM sum.

  ELSEIF p_symbol = '-'.

    PERFORM sub.

  ELSEIF p_symbol = '*'.

    PERFORM multiply.

  ELSEIF p_symbol = '/'.

    PERFORM divide.

  ELSE.

    MESSAGE 'Invalid Operation' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

*The corresponding FORM…ENDFORM blocks are defined at the end of the report.
*When you double-click on a PERFORM statement in the editor,
*the system can automatically generate the related subroutine block.
*Alternatively, you can define it manually.

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
