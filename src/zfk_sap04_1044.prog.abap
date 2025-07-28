*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1039
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1044.
*This is a continuation of report ZFK_SAP04_1040. You can review that report to better understand how to create a Container ALV.

*In an ALV Grid Display, we can represent certain conditions using colors. For example, in our database table, we want to display
*the ticket status in color. If at least 80% of the seats on the flight are sold, we will show it in red; if the sales are
*between 40% and 80%, we will use yellow; and if it’s below 40%, we will show it in green.

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

TYPES: BEGIN OF gty_sflight.
    INCLUDE STRUCTURE sflight.
TYPES:  traffic_light TYPE Icon-id. "To implement this, we add a traffic light column to our table. In order to use icons with their codes, we make use of the ICON-ID type format.
TYPES: END OF gty_sflight.

DATA: gt_sflight TYPE TABLE OF gty_sflight,
      gs_sflight TYPE gty_sflight.

START-OF-SELECTION.

  CALL SCREEN 0100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_1040'.
  SET TITLEBAR 'TITLE_01'.

  PERFORM select_data.
  PERFORM traffic_light. "We create a PERFORM to calculate the percentage values.
  PERFORM fcat.
  PERFORM layout.
  PERFORM show_alv.

ENDMODULE.

MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.
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

*We add an entry to the field catalog in order to display this column.

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
      gs_sflight-traffic_light = '@0A@'. "Icon Code for red
    ELSEIF ( gs_sflight-SEATSOCC / gs_sflight-SEATSMAX ) >= +'0.4' AND ( gs_sflight-SEATSOCC / gs_sflight-SEATSMAX ) < +'0.8'.
      gs_sflight-traffic_light = '@09@'. "Icon Code for yellow
    ELSEIF ( gs_sflight-SEATSOCC / gs_sflight-SEATSMAX ) < +'0.4'.
      gs_sflight-traffic_light = '@08@'. "Icon Code for green
    ENDIF.

    MODIFY gt_sflight FROM gs_sflight TRANSPORTING traffic_light WHERE carrid = gs_sflight-carrid AND
                                                                       connid = gs_sflight-connid AND
                                                                       fldate = gs_sflight-fldate.
    ENDLOOP.
ENDFORM.
