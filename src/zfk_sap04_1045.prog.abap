*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1039
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1045.

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

TYPES: BEGIN OF gty_sflight.
    INCLUDE STRUCTURE sflight.
TYPES:  row_color     TYPE c LENGTH 4.
TYPES:  cell_color    TYPE lvc_t_scol.
TYPES:  traffic_light TYPE Icon-id.
TYPES: END OF gty_sflight.

DATA: gt_sflight TYPE TABLE OF gty_sflight,
      gs_sflight TYPE gty_sflight.

START-OF-SELECTION.

  CALL SCREEN 0100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_1040'.
  SET TITLEBAR 'TITLE_01'.

  PERFORM select_data.
  PERFORM traffic_light.
  PERFORM fcat.
  PERFORM layout.
  PERFORM show_alv.

ENDMODULE.

MODULE user_command_0100 INPUT.

  DATA: lt_selected_rows  TYPE lvc_t_roid,
        ls_selected_row   TYPE lvc_s_roid,
        lt_sflight        TYPE TABLE OF gty_sflight,
        ls_sflight        TYPE gty_sflight,
        lt_index_columns  TYPE lvc_t_col,
        ls_index_column   TYPE lvc_s_col,
        lv_color          TYPE lvc_emphsz,
        lt_selected_cells TYPE lvc_t_cell,
        ls_selected_cell  TYPE lvc_s_cell,
        ls_cell_color     TYPE lvc_s_scol,
        lv_color_number,
        lv_intensified,
        lv_inverse,
        lv_answer.

  CASE sy-ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.
    WHEN 'DELETE'.

      go_alvgrid->get_selected_rows(
        IMPORTING
          et_row_no     = lt_selected_rows
      ).
      lt_sflight = gt_sflight.

      LOOP AT lt_selected_rows INTO ls_selected_row.
        READ TABLE lt_sflight INTO ls_sflight INDEX ls_selected_row-row_id.
        IF sy-subrc IS INITIAL.
          DELETE gt_sflight WHERE carrid = ls_sflight-carrid AND
                                  connid = ls_sflight-connid AND
                                  fldate = ls_sflight-fldate.
        ENDIF.
      ENDLOOP.
    WHEN 'C_COLUMN'.

      go_alvgrid->get_selected_columns(
        IMPORTING
          et_index_columns =     lt_index_columns
      ).
      IF lt_index_columns IS NOT INITIAL.
        CALL FUNCTION 'ZFK_SAP04_F06'
          IMPORTING
            ev_answer = lv_answer
            ev_color  = lv_color.

        IF lv_answer = 0 AND lv_color IS NOT INITIAL.
          LOOP AT lt_index_columns INTO ls_index_column.
            READ TABLE gt_fieldcat INTO gs_fieldcat WITH KEY fieldname = ls_index_column-fieldname.
            IF sy-subrc IS INITIAL.
              gs_fieldcat-emphasize = lv_color.
              MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING emphasize WHERE fieldname = gs_fieldcat-fieldname.
            ENDIF.
          ENDLOOP.
        ELSE.
          MESSAGE 'Please enter color code.' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Please select column.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'C_LINE'.

      go_alvgrid->get_selected_rows(
        IMPORTING
          et_row_no     = lt_selected_rows
      ).

      IF lt_selected_rows IS NOT INITIAL.

        CALL FUNCTION 'ZFK_SAP04_F06'
          IMPORTING
            ev_answer = lv_answer
            ev_color  = lv_color.
        IF lv_answer = 0 AND lv_color IS NOT INITIAL.
          LOOP AT lt_selected_rows INTO ls_selected_row.
            READ TABLE gt_sflight INTO gs_sflight INDEX ls_selected_row-row_id.
            IF sy-subrc IS INITIAL.
              gs_sflight-row_color = lv_color.
              MODIFY gt_sflight FROM gs_sflight TRANSPORTING row_color WHERE carrid = gs_sflight-carrid AND
                                                                             connid = gs_sflight-connid AND
                                                                             fldate = gs_sflight-fldate.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        MESSAGE 'Please select line.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'C_CELL'.
      go_alvgrid->get_selected_cells(
        IMPORTING
          et_cell = lt_selected_cells
      ).

      IF lt_selected_cells IS NOT INITIAL.
        CALL FUNCTION 'ZFK_SAP04_F07'
          IMPORTING
            ev_answer       = lv_answer
            ev_color_number = lv_color_number
            ev_intensified  = lv_intensified
            ev_inverse      = lv_inverse.
        IF lv_answer = 0 AND lv_color_number IS NOT INITIAL AND lv_intensified IS NOT INITIAL AND lv_inverse IS NOT INITIAL.
          LOOP AT lt_selected_cells INTO ls_selected_cell.
            ls_cell_color-fname = ls_selected_cell-col_id.
            ls_cell_color-color-col = lv_color_number.
            ls_cell_color-color-int = lv_intensified.
            ls_cell_color-color-inv = lv_inverse.

            READ TABLE gt_sflight INTO gs_sflight INDEX ls_selected_cell-row_id.
            IF sy-subrc IS INITIAL.
              APPEND ls_cell_color TO gs_sflight-cell_color.

              MODIFY gt_sflight FROM gs_sflight TRANSPORTING cell_color WHERE carrid = gs_sflight-carrid AND
                                                                              connid = gs_sflight-connid AND
                                                                              fldate = gs_sflight-fldate.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.

FORM select_data .

  IF gt_sflight IS INITIAL.
    SELECT * FROM sflight INTO CORRESPONDING FIELDS OF TABLE gt_sflight.
  ENDIF.

ENDFORM.

FORM fcat .

  IF gt_fieldcat IS INITIAL.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'sflight'
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

    gs_fieldcat-fieldname = 'TRAFFIC_LIGHT'.
    gs_fieldcat-col_pos   = 7.
    gs_fieldcat-scrtext_m = 'Ticket Situation'.
    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR gs_fieldcat.

  ENDIF.

ENDFORM.

FORM layout .

  gs_layout-zebra       = abap_true.
  gs_layout-cwidth_opt  = abap_true.
  gs_layout-sel_mode    = 'A'.
  gs_layout-info_fname  = 'ROW_COLOR'.
  gs_layout-ctab_fname  = 'CELL_COLOR'.
ENDFORM.

FORM show_alv .

  IF go_alvgrid IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING
        container_name              = 'CONTAINER_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    CREATE OBJECT go_alvgrid
      EXPORTING
        i_parent          = go_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    go_alvgrid->set_table_for_first_display(
      EXPORTING
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_sflight
        it_fieldcatalog               = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSE.
    go_alvgrid->set_frontend_fieldcatalog( it_fieldcatalog = gt_fieldcat ).

    go_alvgrid->set_frontend_layout( is_layout = gs_layout ).

    go_alvgrid->refresh_table_display(
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.

FORM traffic_light .
  CLEAR: gs_sflight.
  LOOP AT gt_sflight INTO gs_sflight.
    IF ( gs_sflight-SEATSOCC / gs_sflight-SEATSMAX ) >= +'0.8'.
      gs_sflight-traffic_light = '@0A@'.
    ELSEIF ( gs_sflight-SEATSOCC / gs_sflight-SEATSMAX ) >= +'0.4' AND ( gs_sflight-SEATSOCC / gs_sflight-SEATSMAX ) < +'0.8'.
      gs_sflight-traffic_light = '@09@'.
    ELSEIF ( gs_sflight-SEATSOCC / gs_sflight-SEATSMAX ) < +'0.4'.
      gs_sflight-traffic_light = '@08@'.
    ENDIF.

    MODIFY gt_sflight FROM gs_sflight TRANSPORTING traffic_light WHERE carrid = gs_sflight-carrid AND
                                                                       connid = gs_sflight-connid AND
                                                                       fldate = gs_sflight-fldate.
    ENDLOOP.
ENDFORM.
