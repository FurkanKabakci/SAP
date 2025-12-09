*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1057
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1057.

TYPES: BEGIN OF gty_header,
         header TYPE char20,
       END OF gty_header.

DATA: gt_scarr    TYPE TABLE OF scarr,
      gt_header   TYPE TABLE OF gty_header,
      gv_filename TYPE string.

PARAMETERS: p_path TYPE string.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  cl_gui_frontend_services=>directory_browse(
    CHANGING
      selected_folder      = p_path
    EXCEPTIONS
      cntl_error           = 1                " Control error
      error_no_gui         = 2                " No GUI available
      not_supported_by_gui = 3                " GUI does not support this
      OTHERS               = 4
  ).
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  gv_filename = p_path && '\' && sy-datum && sy-uzeit && '.XLSX'.

START-OF-SELECTION.

  SELECT * FROM scarr INTO TABLE gt_scarr.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = DATA(go_salv)
        CHANGING
          t_table        = gt_scarr
      ).
    CATCH cx_salv_msg. " ALV: General Error Class with Message
  ENDTRY.

  DATA(gv_scarr_xstring) = go_salv->to_xml( xml_type    = if_salv_bs_xml=>c_type_xlsx ).

  DATA(gt_scarr_solixtab) = cl_bcs_convert=>xstring_to_solix( iv_xstring = gv_scarr_xstring ).


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize            = xstrlen( gv_scarr_xstring )
      filename                = gv_filename
      filetype                = 'BIN'

    TABLES
      data_tab                = gt_scarr_solixtab
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.
