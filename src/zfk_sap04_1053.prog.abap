*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1053
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1053.

*A modeless dialog box allows the user to interact with other screens or windows
*while it is open. In other words, it is “independent” and does not block user actions
*on other parts of the application.
*	•	Usage: Standard dynpro screens in ABAP are usually modal. To create a modeless dialog,
*  you typically use ABAP Objects GUI classes:
*	•	CL_GUI_DIALOGBOX_CONTAINER – Opens the dialog in a container.
*	•	CL_GUI_CUSTOM_CONTAINER – Manages the dialog in a custom container.
*	•	Advantages: The user can work with multiple windows or screens simultaneously.
*	•	Disadvantages: Event handling and coding are more complex compared to modal dialogs.

TYPES: BEGIN OF gty_str,
         box.
    INCLUDE STRUCTURE scarr.
TYPES: END OF gty_str.

DATA: gt_scarr  TYPE TABLE OF gty_str,
      gs_scarr  TYPE gty_str,
      gt_fcat   TYPE slis_t_fieldcat_alv,
      gs_layout TYPE slis_layout_alv.

*To enable the Close button of a dialog box created with CL_GUI_DIALOGBOX_CONTAINER,
*an event handler class is implemented to handle the close event.

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS on_close
      FOR EVENT close OF cl_gui_dialogbox_container
      IMPORTING
        !sender .
ENDCLASS.

CLASS lcl_events IMPLEMENTATION.

  METHOD on_close.

    IF sender IS NOT INITIAL.
      sender->free( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

SELECT * FROM scarr INTO CORRESPONDING FIELDS OF TABLE gt_scarr.

*  We used the REUSE_ALV_GRID_DISPLAY function module for our main table.

CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
    i_structure_name       = 'SCARR'
    i_bypassing_buffer     = abap_true
  CHANGING
    ct_fieldcat            = gt_fcat
  EXCEPTIONS
    inconsistent_interface = 1
    program_error          = 2
    OTHERS                 = 3.
IF sy-subrc <> 0.
  BREAK-POINT.
ENDIF.

gs_layout-zebra             = abap_true.
gs_layout-colwidth_optimize = abap_true.
gs_layout-box_fieldname     = 'BOX'.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program       = sy-repid
    i_callback_pf_status_set = 'PF_STATUS_1053 '
    i_callback_user_command  = 'UC_1053'
    is_layout                = gs_layout
    it_fieldcat              = gt_fcat
  TABLES
    t_outtab                 = gt_scarr
  EXCEPTIONS
    program_error            = 1
    OTHERS                   = 2.
IF sy-subrc <> 0.
  BREAK-POINT.
ENDIF.

FORM pf_status_1053 USING lt_extab TYPE slis_t_extab.
  SET PF-STATUS 'PF_STATUS_1053'.
ENDFORM.

FORM uc_1053 USING lv_ucomm    TYPE sy-ucomm
                   ls_selfield TYPE slis_selfield.
  CASE lv_ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.

* We create a modeless dialog box that displays the details when a row is double-clicked.
    WHEN '&IC1'.

      PERFORM modeless_dialogbox USING ls_selfield-tabindex.
  ENDCASE.

ENDFORM.

FORM modeless_dialogbox USING ls_selfield_tabindex.
  DATA(lv_carrid) = gt_scarr[ ls_selfield_tabindex ]-carrid.
  SELECT * FROM sflight INTO TABLE @DATA(gt_sflight) WHERE carrid = @lv_carrid.

* To create a modeless dialog box, we use the CL_GUI_DIALOGBOX_CONTAINER class.

  DATA(lo_dialogbox) = NEW cl_gui_dialogbox_container(
      width                       = 1000
      height                      = 300
      top                         = 30
      left                        = 80
      no_autodef_progid_dynnr     = abap_true
  ).

    SET HANDLER lcl_events=>on_close FOR lo_dialogbox.

*Here, we create the modeless dialog box using SALV.
*This way, the report uses both REUSE_ALV_GRID_DISPLAY and SALV.

    TRY.
    cl_salv_table=>factory(
      EXPORTING
        r_container    = lo_dialogbox
      IMPORTING
        r_salv_table   = DATA(lo_salv)
      CHANGING
        t_table        = gt_sflight
    ).
      CATCH cx_salv_msg.
    ENDTRY.

    lo_salv->display( ).
ENDFORM.
