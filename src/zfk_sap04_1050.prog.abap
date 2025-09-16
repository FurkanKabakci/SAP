*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1050
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFK_SAP04_1050.

*Checkboxes are normally used for multiple selections.
*However, sometimes a single-selection behavior (like a radiobutton) is desired. To achieve this:
* 1.  Assign a User Command: A user command is assigned to the checkboxes.
* 2.  Immediate Updates: The user’s selections should update immediately on the screen.
*  For this, the AT SELECTION-SCREEN event is used. When a selection changes on the screen,
*  the corresponding logic is executed to update the checkboxes.
*Using this method, a checkbox behaves like a radiobutton, allowing only one selection at a time.

SELECTION-SCREEN BEGIN OF BLOCK cbox WITH FRAME.
PARAMETERS: p_cbox1 AS CHECKBOX USER-COMMAND cbox1 DEFAULT abap_true,
            p_cbox2 AS CHECKBOX USER-COMMAND cbox2,
            p_cbox3 AS CHECKBOX USER-COMMAND cbox3.
SELECTION-SCREEN END OF BLOCK cbox.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'CBOX1'.
      p_cbox1 = abap_true.
      p_cbox2 = abap_false.
      p_cbox3 = abap_false.
    WHEN 'CBOX2'.
      p_cbox1 = abap_false.
      p_cbox2 = abap_true.
      p_cbox3 = abap_false.
    WHEN 'CBOX3'.
      p_cbox1 = abap_false.
      p_cbox2 = abap_false.
      p_cbox3 = abap_true.
  ENDCASE.
