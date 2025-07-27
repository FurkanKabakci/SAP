*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1039
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1043.

*This is a continuation of report ZFK_SAP04_1040. You can review that report to better understand how to create a Container ALV.

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

*For cell-level coloring, we append a cell_color column to the internal table based on the SFLIGHT structure,
*and assign this field to gs_layout-ctab_fname so that it won’t appear in the ALV output

TYPES: BEGIN OF gty_sflight.
    INCLUDE STRUCTURE sflight.
TYPES:  cell_color    TYPE lvc_t_scol. "The cell_color field, which is appended to the end of the table, holds an
"internal table with color information. This represents a typical example of a deep structure in ABAP.
TYPES: END OF gty_sflight.

DATA: gt_sflight TYPE TABLE OF gty_sflight,
      gs_sflight TYPE gty_sflight.

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

*In order to capture the cells selected by the user in the ALV table, we declare an internal table of type LVC_T_CELL,
*and then call the method go_alvgrid->get_selected_cells to retrieve the selected cell information.

*Furthermore, the color information requested from the user is received in three components:
*the color number, the intensified indicator, and the inverse indicator.

  DATA: lt_selected_cells TYPE lvc_t_cell,
        ls_selected_cell  TYPE lvc_s_cell,
        ls_cell_color     TYPE lvc_s_scol,
        lv_color_number,
        lv_intensified,
        lv_inverse,
        lv_answer.

  CASE sy-ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.

    WHEN 'C_CELL'.
      go_alvgrid->get_selected_cells(
        IMPORTING
          et_cell = lt_selected_cells
      ).

      IF lt_selected_cells IS NOT INITIAL.
        CALL FUNCTION 'ZFK_SAP04_F07' "The function module ZFK_SAP04_F07, which is used to collect input from the user, can be examined in the repository.
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

  ENDIF.

ENDFORM.

FORM layout .

  gs_layout-zebra       = abap_true.
  gs_layout-cwidth_opt  = abap_true.
  gs_layout-sel_mode    = 'A'.
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
