*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_2004
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_2004.

TYPES: BEGIN OF gty_str,
         box.
    INCLUDE STRUCTURE zfk_stravelag.
TYPES: END OF gty_str.

DATA: gt_tbl      TYPE TABLE OF gty_str,
      gs_str      TYPE gty_str,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.

SELECT-OPTIONS: so_agenc FOR gs_str-agencynum.


PERFORM get_data.

PERFORM get_alv.


*  cl_demo_output=>display( gt_tbl ).

FORM get_data .
  SELECT * FROM zfk_stravelag INTO CORRESPONDING FIELDS OF TABLE gt_tbl WHERE agencynum IN so_agenc.
ENDFORM.

FORM get_alv .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'zfk_stravelag'
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

  LOOP AT gt_fieldcat INTO gs_fieldcat.
    IF gs_fieldcat-fieldname = 'URL'.
      gs_fieldcat-hotspot = abap_true.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING hotspot WHERE fieldname = 'URL'.
    ENDIF.

  ENDLOOP.

  gs_layout-zebra             = abap_true.
  gs_layout-colwidth_optimize = abap_true.
  gs_layout-box_fieldname     = 'BOX'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_2004 '
      i_callback_user_command  = 'UC_2004'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
    TABLES
      t_outtab                 = gt_tbl
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


ENDFORM.

FORM pf_2004 USING lt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STATUS_2004'.
ENDFORM.

FORM uc_2004 USING lv_ucomm    TYPE sy-ucomm
                   ls_selfield TYPE slis_selfield.
  DATA: lv_lines   TYPE i,
        lt_city    TYPE TABLE OF city,
        lv_city_nu TYPE i,
        lt_pop_up  TYPE TABLE OF gty_str,
        lv_url     TYPE s_url,
        lv_answer  TYPE c LENGTH 1.

  CASE lv_ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.
    WHEN 'LINES'.
      lv_lines = lines( gt_tbl ).

      MESSAGE |Number of lines: { lv_lines } | TYPE 'I'.
    WHEN 'CITY'.
      LOOP AT gt_tbl INTO gs_str.
        APPEND gs_str-city TO lt_city.
      ENDLOOP.
      SORT lt_city.
      DELETE ADJACENT DUPLICATES FROM lt_city.
      lv_city_nu = lines( lt_city ).
      MESSAGE |Number of city: { lv_city_nu } | TYPE 'I'.

    WHEN 'INFO'.
      CLEAR: gs_str, lt_pop_up.

      LOOP AT gt_tbl INTO gs_str.
        IF gs_str-box = abap_true.
          APPEND gs_str TO lt_pop_up.
        ENDIF.
      ENDLOOP.
      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
        EXPORTING
          i_title               = 'INFO'
          i_screen_start_column = 5
          i_screen_start_line   = 5
          i_screen_end_column   = 165
          i_screen_end_line     = 8
          i_tabname             = 'lt_pop_up'
          it_fieldcat           = gt_fieldcat
          i_callback_program    = sy-repid
*       IMPORTING
*         ES_SELFIELD           =
*         E_EXIT                =
        TABLES
          t_outtab              = lt_pop_up
        EXCEPTIONS
          program_error         = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    WHEN '&IC1'.
      IF ls_selfield-fieldname = 'URL'.

        CALL FUNCTION 'ZFK_SAP04_F04'
          IMPORTING
            ev_url    = lv_url
            ev_answer = lv_answer.

        IF lv_answer = 0 AND lv_url IS NOT INITIAL.
          READ TABLE gt_tbl INTO gs_str INDEX ls_selfield-tabindex.

          IF sy-subrc IS INITIAL.
            UPDATE zfk_stravelag SET old_url = gs_str-url WHERE url = ls_selfield-value.

            gs_str-url = lv_url.

            UPDATE zfk_stravelag SET url = gs_str-url WHERE url = ls_selfield-value.
          ENDIF.

          SELECT * FROM zfk_stravelag
            INTO CORRESPONDING FIELDS OF TABLE gt_tbl WHERE agencynum IN so_agenc .

          ls_selfield-refresh = abap_true.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.
