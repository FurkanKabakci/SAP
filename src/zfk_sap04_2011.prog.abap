*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_2011
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_2011.

*Prepare a Container ALV for the SCARR table.
*Create two buttons.
*When the first button is clicked, highlight the odd-numbered rows in the ALV with a color of your choice.
*When the second button is clicked, highlight the even-numbered rows with a different color.

TYPES: BEGIN OF gty_scarr.
        INCLUDE STRUCTURE scarr.
TYPES   row_color TYPE c LENGTH 4.
TYPES: END OF gty_scarr.

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_scarr     TYPE TABLE OF gty_scarr,
      gs_scarr     TYPE gty_scarr,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

CALL SCREEN 0100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_2011'.
  SET TITLEBAR 'TITLE-001'.

  PERFORM get_data.
  PERFORM fcat.
  PERFORM layout.
  PERFORM show_alv.

ENDMODULE.

MODULE user_command_0100 INPUT.

  DATA: lv_color TYPE c LENGTH 4,
        lv_answer.

  CASE sy-ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.

    WHEN 'C_ODD_LINES'.
      CALL FUNCTION 'ZFK_SAP04_F06'
           IMPORTING
             EV_ANSWER       = lv_answer
             EV_COLOR        = lv_color
             .
      IF lv_answer = 0 AND lv_color IS NOT INITIAL.
        LOOP AT gt_scarr INTO gs_scarr.
          IF sy-tabix MOD 2 = 1.
             gs_scarr-row_color = lv_color.
             MODIFY gt_scarr FROM gs_scarr TRANSPORTING row_color WHERE carrid = gs_scarr-carrid AND
                                                                        carrname = gs_scarr-carrname.
          ENDIF.
       ENDLOOP.
      ELSE.
        MESSAGE | Please fill in the color box. | TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'C_EVEN_LINES'.
      CALL FUNCTION 'ZFK_SAP04_F06'
           IMPORTING
             EV_ANSWER       = lv_answer
             EV_COLOR        = lv_color
             .
      IF lv_answer = 0 AND lv_color IS NOT INITIAL.
        LOOP AT gt_scarr INTO gs_scarr.
          IF sy-tabix MOD 2 = 0.
             gs_scarr-row_color = lv_color.
             MODIFY gt_scarr FROM gs_scarr TRANSPORTING row_color WHERE carrid = gs_scarr-carrid AND
                                                                        carrname = gs_scarr-carrname.
          ENDIF.
       ENDLOOP.
      ELSE.
        MESSAGE | Please fill in the color box. | TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.

ENDMODULE.

FORM get_data .
  IF gt_scarr IS INITIAL.
    SELECT * FROM scarr INTO CORRESPONDING FIELDS OF TABLE gt_scarr.
  ENDIF.
ENDFORM.

FORM fcat .
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
   EXPORTING
     I_STRUCTURE_NAME             = 'scarr'
     I_BYPASSING_BUFFER           = abap_true
    CHANGING
      ct_fieldcat                  = gt_fieldcat
   EXCEPTIONS
     INCONSISTENT_INTERFACE       = 1
     PROGRAM_ERROR                = 2
     OTHERS                       = 3
            .
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.

FORM layout .
  gs_layout-zebra = abap_true.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-info_fname = 'ROW_COLOR'.
ENDFORM.

FORM show_alv .
  IF go_alvgrid IS INITIAL.

    CREATE OBJECT go_container
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6
      .
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
        others            = 5
      .
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    go_alvgrid->set_table_for_first_display(
      EXPORTING
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_scarr
        it_fieldcatalog               = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4
    ).
    IF sy-subrc <> 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    go_alvgrid->set_frontend_fieldcatalog( it_fieldcatalog = gt_fieldcat  ).

    go_alvgrid->set_frontend_layout( is_layout = gs_layout ).

    go_alvgrid->refresh_table_display(
      EXCEPTIONS
        finished       = 1
        others         = 2
    ).
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.
