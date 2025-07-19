*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_2009
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_2009.

DATA: gs_str     TYPE scarr,
      gt_scarr   TYPE TABLE OF scarr,
      gt_spfli   TYPE TABLE OF spfli,
      gt_sflight TYPE TABLE OF sflight,
      go_get_data TYPE REF TO ZFK_CL_SAP04_008.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
SELECT-OPTIONS: so_carid FOR gs_str-carrid.
PARAMETERS: p_scarr   AS CHECKBOX,
            p_spfli   AS CHECKBOX,
            p_sflght AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK a1.

START-OF-SELECTION.

CREATE OBJECT go_get_data.

go_get_data->get_data(
  EXPORTING
    it_carrid  = so_carid[]
    iv_scarr   = p_scarr
    iv_spfli   = p_spfli
    iv_sflight = p_sflght
  IMPORTING
    et_scarr   = gt_scarr
    et_spfli   = gt_spfli
    et_sflight = gt_sflight
).

cl_demo_output=>display( gt_scarr ).
cl_demo_output=>display( gt_spfli ).
cl_demo_output=>display( gt_sflight ).
