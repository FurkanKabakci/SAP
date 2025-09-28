*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1054
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1054.

TYPES: BEGIN OF gty_header,
         header TYPE c LENGTH 20,
       END OF gty_header.

DATA: gt_header TYPE TABLE OF gty_header,
      gv_desktop_path TYPE string,
      gv_full_path TYPE string.


START-OF-SELECTION.

SELECT * FROM scarr INTO TABLE @DATA(gt_tbl).

gt_header = VALUE #( ( header = 'MANDT'    ) ( header = 'CARRID'   )
                     ( header = 'CARRNAME' ) ( header = 'CURRCODE' )
                     ( header = 'URL'      ) ).

cl_gui_frontend_services=>get_desktop_directory(
  CHANGING
    desktop_directory    = gv_desktop_path
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    others               = 4
).
IF sy-subrc <> 0.
  BREAK-POINT.
ENDIF.

*gv_full_path = gv_desktop_path && '\Scarr_excel.XLS'.
gv_full_path = '\\Mac\Home\Desktop' && '\Scarr_excel.XLS'.

cl_gui_frontend_services=>gui_download(
  EXPORTING
*    bin_filesize              =     " File length for binary files
    filename                  = gv_full_path
    filetype                  = 'ASC'
*    append                    = SPACE    " Character Field of Length 1
    write_field_separator     = abap_true
*    header                    = '00'    " Byte Chain Written to Beginning of File in Binary Mode
*    trunc_trailing_blanks     = SPACE    " Do not Write Blank at the End of Char Fields
*    write_lf                  = 'X'    " Insert CR/LF at End of Line in Case of Char Download
*    col_select                = SPACE    " Copy Only Selected Columns of the Table
*    col_select_mask           = SPACE    " Vector Containing an 'X' for the Column To Be Copied
*    dat_mode                  = SPACE    " Numeric and date fields are in DAT format in WS_DOWNLOAD
    confirm_overwrite         = abap_true
*    no_auth_check             = SPACE    " Switch off Check for Access Rights
*    codepage                  =     " Character Representation for Output
*    ignore_cerr               = ABAP_TRUE    " Ignore character set conversion errors?
*    replacement               = '#'    " Replacement Character for Non-Convertible Characters
*    write_bom                 = SPACE    " If set, writes a Unicode byte order mark
*    trunc_trailing_blanks_eol = 'X'    " Remove Trailing Blanks in Last Column
*    wk1_n_format              = SPACE
*    wk1_n_size                = SPACE
*    wk1_t_format              = SPACE
*    wk1_t_size                = SPACE
*    show_transfer_status      = 'X'    " Enables suppression of transfer status message
    fieldnames                = gt_header
*    write_lf_after_last_line  = 'X'    " Writes a CR/LF after final data record
*    virus_scan_profile        = '/SCET/GUI_DOWNLOAD'    " Virus Scan Profile
*  IMPORTING
*    filelength                =     " Number of bytes transferred
  CHANGING
    data_tab                  = gt_tbl
  EXCEPTIONS
    file_write_error          = 1
    no_batch                  = 2
    gui_refuse_filetransfer   = 3
    invalid_type              = 4
    no_authority              = 5
    unknown_error             = 6
    header_not_allowed        = 7
    separator_not_allowed     = 8
    filesize_not_allowed      = 9
    header_too_long           = 10
    dp_error_create           = 11
    dp_error_send             = 12
    dp_error_write            = 13
    unknown_dp_error          = 14
    access_denied             = 15
    dp_out_of_memory          = 16
    disk_full                 = 17
    dp_timeout                = 18
    file_not_found            = 19
    dataprovider_exception    = 20
    control_flush_error       = 21
    not_supported_by_gui      = 22
    error_no_gui              = 23
    others                    = 24
).
IF sy-subrc <> 0.
  BREAK-POINT.
ENDIF.

WAIT UP TO 1 SECONDS.

cl_gui_frontend_services=>execute(
  EXPORTING
    document               = gv_full_path
  EXCEPTIONS
    cntl_error             = 1
    error_no_gui           = 2
    bad_parameter          = 3
    file_not_found         = 4
    path_not_found         = 5
    file_extension_unknown = 6
    error_execute_failed   = 7
    synchronous_failed     = 8
    not_supported_by_gui   = 9
    others                 = 10
).
IF sy-subrc <> 0.
  BREAK-POINT.
ENDIF.
