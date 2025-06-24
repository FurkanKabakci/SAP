*&---------------------------------------------------------------------*
*&  Include           ZFK_SAP04_1025_TOP
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001 NO INTERVALS.
PARAMETERS: p_1      TYPE i,
            p_2      TYPE i,
            p_symbol TYPE c LENGTH 1.
SELECTION-SCREEN END OF BLOCK a1.

DATA: gv_result TYPE p DECIMALS 2.
