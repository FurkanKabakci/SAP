*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1041
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1041.

*This is a continuation of report ZFK_SAP04_1040. You can review that report to better understand how to create a Container ALV.

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

DATA: gt_sflight TYPE TABLE OF sflight,
      gs_sflight TYPE sflight.

START-OF-SELECTION.

  CALL SCREEN 0100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_1040'.
  SET TITLEBAR 'TITLE_01'.

  PERFORM select_data.
  PERFORM fcat.
  PERFORM layout.
  PERFORM show_alv.

ENDMODULE.

MODULE user_command_0100 INPUT.

  DATA: lt_selected_rows  TYPE lvc_t_roid,
        ls_selected_row   TYPE lvc_s_roid,
        lt_sflight        TYPE TABLE OF sflight,
        ls_sflight        TYPE sflight,
        lt_index_columns  TYPE lvc_t_col, "With this setup and the method go_alvgrid->get_selected_columns,
        "we can retrieve the columns selected by the user as an internal table.
        ls_index_column   TYPE lvc_s_col,
        lv_color          TYPE lvc_emphsz,
        lv_answer.

  CASE sy-ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.
    WHEN 'C_COLUMN'.

      go_alvgrid->get_selected_columns(
        IMPORTING
          et_index_columns =     lt_index_columns
      ).
      IF lt_index_columns IS NOT INITIAL.
        CALL FUNCTION 'ZFK_SAP04_F06' "You can access this function from the repository. Using the selection screen, we collect the color code and the user’s confirmation status.
          IMPORTING
            ev_answer = lv_answer
            ev_color  = lv_color. "You can observe the highlighting effect by entering a color code such as C711.

        IF lv_answer = 0 AND lv_color IS NOT INITIAL.
          LOOP AT lt_index_columns INTO ls_index_column.
            READ TABLE gt_fieldcat INTO gs_fieldcat WITH KEY fieldname = ls_index_column-fieldname.
            IF sy-subrc IS INITIAL.
              gs_fieldcat-emphasize = lv_color. "The color code entered in the emphasize field of the field catalog will cause the corresponding column to be highlighted.
              MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING emphasize WHERE fieldname = gs_fieldcat-fieldname.
            ENDIF.
          ENDLOOP.
        ELSE.
          MESSAGE 'Please enter color code.' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Please select column.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
ENDMODULE.

FORM select_data .

  IF gt_sflight IS INITIAL.
    SELECT * FROM sflight INTO TABLE gt_sflight.
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

  ENDIF.

ENDFORM.

FORM layout .

  gs_layout-zebra       = abap_true.
  gs_layout-cwidth_opt  = abap_true.
  gs_layout-sel_mode    = 'A'.


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
*    We use the methods go_alvgrid->set_frontend_fieldcatalog and go_alvgrid->set_frontend_layout to instantly
*    reflect and observe any changes made to the field catalog in the ALV grid.
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
