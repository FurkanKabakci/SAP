*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1039
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1040.

*In SAP ABAP, Container ALV is a type of ALV (ABAP List Viewer) that displays
*the ALV output within a specific area (container) of a screen.

*Container ALV allows the ALV grid to be displayed inside a screen element
*(for example, a Custom Control). It is created using the class CL_GUI_ALV_GRID
*and typically works together with CL_GUI_CUSTOM_CONTAINER.

*Container ALV offers a more flexible structure compared to classic ALV and is
*used in Dynpro screens or Object-Oriented GUI applications. It is especially
*preferred in interactive applications.

DATA: go_container TYPE REF TO cl_gui_custom_container,  "Provides access to the custom control on the screen.
      go_alvgrid   TYPE REF TO cl_gui_alv_grid,          "Creates the ALV grid object.
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

DATA: gt_sflight TYPE TABLE OF sflight,
      gs_sflight TYPE sflight.

START-OF-SELECTION.

*CALL SCREEN is used to call a screen (Dynpro). It provides the user with a graphical interface where
*an ALV Grid can be embedded using a custom control. This is a fundamental step when displaying
*a container-based ALV. Additionally, the screen can include buttons and other UI elements to
*enhance user interaction.

  CALL SCREEN 0100.

*MODULE OUTPUT runs before the screen (CALL SCREEN) is displayed, and is typically used to prepare and show data (e.g., ALV grid).
*MODULE INPUT runs after the user interacts with the screen and is used to handle user input or actions.

*These modules are activated in the screen’s Flow Logic section under PROCESS BEFORE OUTPUT and PROCESS AFTER INPUT.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_1040'.
  SET TITLEBAR 'TITLE_01'.

  PERFORM select_data.
  PERFORM fcat.
  PERFORM layout.
  PERFORM show_alv.

ENDMODULE.

MODULE user_command_0100 INPUT.

  DATA: lt_selected_rows  TYPE lvc_t_roid,  "With this setup and the method go_alvgrid->get_selected_rows, we can retrieve the rows selected by the user as an internal table.
        ls_selected_row   TYPE lvc_s_roid,
        lt_sflight        TYPE TABLE OF sflight,
        ls_sflight        TYPE sflight.

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

    go_alvgrid->set_table_for_first_display(    "Used to bind the data to the ALV.
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

    go_alvgrid->refresh_table_display( "With the method go_alvgrid->refresh_table_display, we ensure that the ALV is refreshed and shows the updated view after the user performs an action.
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.
