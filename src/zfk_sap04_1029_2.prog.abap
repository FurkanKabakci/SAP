*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1029_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1029_2.

DATA: gt_tbl      TYPE TABLE OF sbook,
      gs_str      TYPE sbook,
      gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo.


SELECT * FROM sbook INTO TABLE gt_tbl.

CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
  EXPORTING
    i_structure_name       = 'sbook'
    i_bypassing_buffer     = abap_true
  CHANGING
    ct_fieldcat            = gt_fieldcat
  EXCEPTIONS
    inconsistent_interface = 1
    program_error          = 2
    OTHERS                 = 3.
IF sy-subrc <> 0.
  EXIT.
ENDIF.

gs_layout-zebra       = abap_true.
gs_layout-cwidth_opt  = abap_true.
gs_layout-sel_mode    = 'A'.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
  EXPORTING
    i_callback_program       = sy-repid
    is_layout_lvc            = gs_layout
    it_fieldcat_lvc          = gt_fieldcat
  TABLES
    t_outtab                 = gt_tbl
  EXCEPTIONS
    program_error            = 1
    OTHERS                   = 2.
IF sy-subrc <> 0.
  EXIT.
ENDIF.
