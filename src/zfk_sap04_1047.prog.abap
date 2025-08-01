*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1047
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1047.

*
*In SAP ABAP, a Dialog Screen is an interface that enables interaction between the user and the system.
*It is used for data entry, making selections, or initiating specific processes.

*What is a Dialog Screen?

*A dialog screen, also known in SAP as a dynpro (dynamic program), contains UI elements such as input fields,
*buttons, and tables. It works in conjunction with ABAP programs and is commonly used in module pool programming.
*	•	Screens are numbered (e.g., 0100, 0200).
*	•	Each screen has a layout (visual design) and a flow logic section.
*	•	It can display information to the user or receive input from the user.
*	•	Operates through PBO (Process Before Output) and PAI (Process After Input) events.

*Just like in container-based ALV reports, dialog screens are called using the CALL SCREEN statement.
*DYNPRO screens can be used to match or interact with data within a report.

*In this example, interactive CRUD operations (Create - Read - Update - Delete)
*and ALV display have also been implemented with user interaction.

DATA: gv_id       TYPE n LENGTH 10,
      gv_name     TYPE c LENGTH 40,
      gv_surname  TYPE c LENGTH 40,
      gv_job      TYPE c LENGTH 40,
      gv_salary   TYPE i,
      gv_currency TYPE c LENGTH 3,
      gv_email    TYPE c LENGTH 40.

DATA: gt_tbl TYPE TABLE OF zfk_sap_tbl_01,
      gs_str TYPE zfk_sap_tbl_01.

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

DATA: gv_user TYPE sy-uname,
      gv_date TYPE sy-datum,
      gv_time TYPE sy-uzeit.

DATA: gv_lines    TYPE i,
      gv_info(60) TYPE c.

CALL SCREEN 0100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_1047'.
  SET TITLEBAR '1047'.

  PERFORM get_data.
  IF go_alvgrid IS NOT INITIAL.
    go_alvgrid->refresh_table_display(
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
ENDMODULE.

MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.
    WHEN 'CREATE'.
      READ TABLE gt_tbl INTO gs_str WITH KEY id = gv_id.
      IF sy-subrc IS NOT INITIAL.
        IF gv_id IS NOT INITIAL AND
         gv_name IS NOT INITIAL AND
         gv_surname IS NOT INITIAL AND
         gv_job IS NOT INITIAL AND
         gv_salary IS NOT INITIAL AND
         gv_currency IS NOT INITIAL AND
         gv_email IS NOT INITIAL.

          gs_str-id       =  gv_id.
          gs_str-name     =  gv_name.
          gs_str-surname  =  gv_surname.
          gs_str-job      =  gv_job.
          gs_str-salary   =  gv_salary.
          gs_str-currency =  gv_currency.
          gs_str-email    =  gv_email.

          INSERT zfk_sap_tbl_01 FROM gs_str.
          MESSAGE | The user was successfully created. | TYPE 'S'.
          CLEAR: gs_str.
        ELSE.
          MESSAGE | You must fill all empty places! | TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE | The user has already been  created! | TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'READ'.
      IF gv_id IS NOT INITIAL.
        READ TABLE gt_tbl INTO gs_str WITH KEY id = gv_id.
        IF sy-subrc IS INITIAL.

          gv_name     =    gs_str-name.
          gv_surname  =    gs_str-surname.
          gv_job      =    gs_str-job.
          gv_salary   =    gs_str-salary.
          gv_currency =    gs_str-currency.
          gv_email    =    gs_str-email.

          CLEAR: gs_str.
        ELSE.
          MESSAGE | The user was not found! | TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE | You must fill id! | TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'UPDATE'.
      IF gv_id IS NOT INITIAL.
        READ TABLE gt_tbl INTO gs_str WITH KEY id = gv_id.
        IF sy-subrc IS INITIAL.
          IF gv_name IS NOT INITIAL.
            gs_str-name        =   gv_name.
          ENDIF.
          IF gv_name IS NOT INITIAL.
            gs_str-name        =   gv_name.
          ENDIF.
          IF gv_surname IS NOT INITIAL.
            gs_str-surname     =   gv_surname.
          ENDIF.
          IF gv_job IS NOT INITIAL.
            gs_str-job         =   gv_job.
          ENDIF.
          IF gv_salary IS NOT INITIAL.
            gs_str-salary      =   gv_salary.
          ENDIF.
          IF gv_currency IS NOT INITIAL.
            gs_str-currency    =   gv_currency.
          ENDIF.
          IF gv_email IS NOT INITIAL.
            gs_str-email       =   gv_email.
          ENDIF.

          MODIFY zfk_sap_tbl_01 FROM gs_str.
          CLEAR: gs_str.
          MESSAGE | The user was successfully updated. | TYPE 'S'.
        ELSE.
          MESSAGE | The user was not found! | TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE | You must fill id! | TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'DELETE'.
      IF gv_id IS NOT INITIAL.
        READ TABLE gt_tbl INTO gs_str WITH KEY id = gv_id.
        IF sy-subrc IS INITIAL.
          DELETE FROM zfk_sap_tbl_01 WHERE id = gv_id.
          MESSAGE | The user was successfully deleted. | TYPE 'S'.
        ELSE.
          MESSAGE | The user was not found! | TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE | You must fill id! | TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'ALV'.
      PERFORM get_data.
      PERFORM fcat.
      PERFORM layout.
      PERFORM show_alv.

      gv_user =  sy-uname.
      gv_date =  sy-datum.
      gv_time =  sy-uzeit.

      DESCRIBE TABLE gt_tbl LINES gv_lines.
      gv_info    = | This report has { gv_lines } items. |.

  ENDCASE.

ENDMODULE.

FORM get_data .
  SELECT * FROM zfk_sap_tbl_01 INTO TABLE gt_tbl.

ENDFORM.

FORM fcat .
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'zfk_sap_tbl_01'
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

ENDFORM.

FORM layout .
  gs_layout-zebra      = abap_true.
  gs_layout-cwidth_opt = abap_true.

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
        it_outtab                     = gt_tbl
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
    go_alvgrid->check_changed_data( ).
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
