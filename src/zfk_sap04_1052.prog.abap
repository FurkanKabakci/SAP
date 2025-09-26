*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1051
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1052.

DATA: gt_vbak      TYPE TABLE OF vbak,
      gt_vbap      TYPE TABLE OF vbap,
      gt_vbeln     TYPE TABLE OF vbeln,
      go_salv      TYPE REF TO cl_salv_table,
      go_salv_item TYPE REF TO cl_salv_table,
      go_cont_item TYPE REF TO cl_gui_container.

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
            e_salv_function
            sender.
* doppelklick
    CLASS-METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
          row
          column
          sender.
* Link-Klick
    CLASS-METHODS: on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
          row
          column
          sender.

    CLASS-METHODS on_close
          FOR EVENT close OF cl_gui_dialogbox_container
      IMPORTING
          !sender .

ENDCLASS.

CLASS lcl_events IMPLEMENTATION.

  METHOD on_added_function.

    DATA(lt_selected_rows) = go_salv->get_selections( )->get_selected_rows( ).

    CLEAR gt_vbeln.

    LOOP AT lt_selected_rows INTO DATA(ls_selected_row).
      APPEND gt_vbak[ ls_selected_row ]-vbeln TO gt_vbeln.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click.

    CHECK row > 0.

    DATA(lv_vbeln) = gt_vbak[ row ]-vbeln.

    SELECT * FROM vbap INTO TABLE gt_vbap WHERE vbeln = lv_vbeln.

      DATA(lt_selected_rows) = go_salv->get_selections( )->get_selected_rows( ).

      IF go_salv_item IS NOT BOUND.

      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = go_cont_item
            IMPORTING
              r_salv_table   = go_salv_item
            CHANGING
              t_table        = gt_vbap
          ).
        CATCH cx_salv_msg.
      ENDTRY.

      go_salv_item->get_display_settings( )->set_striped_pattern( abap_true ).
      go_salv_item->display( ).

      ELSE.
        go_salv_item->refresh( ).

      ENDIF.

  ENDMETHOD.

  METHOD on_link_click.

    BREAK-POINT.

  ENDMETHOD.


  METHOD on_close.

    IF sender IS NOT INITIAL.
      sender->free( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_user TYPE ernam.
SELECTION-SCREEN END OF BLOCK a1.

START-OF-SELECTION.

  CALL SCREEN 100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_100'.
*  SET TITLEBAR 'xxx'.
  PERFORM get_data.
  PERFORM create_splitter_alv.

ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'LEAVE'.
      LEAVE TO SCREEN 0.
    WHEN 'ITEM'.

  ENDCASE.

ENDMODULE.

FORM get_data .
  SELECT * FROM vbak INTO TABLE gt_vbak WHERE ernam = p_user.
ENDFORM.

FORM create_splitter_alv .

  DATA(lo_main_cont) = NEW cl_gui_custom_container(
      container_name              = 'MAIN_CONT'
  ).

  DATA(lo_splitter) = NEW cl_gui_splitter_container(
      parent                  = lo_main_cont
      rows                    = 2
      columns                 = 1
      no_autodef_progid_dynnr = abap_true
  ).

  DATA(lo_cont_header) = lo_splitter->get_container(
                           row       = 1
                           column    = 1
                       ).

  go_cont_item = lo_splitter->get_container(
                         row       = 2
                         column    = 1
                     ).

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = lo_cont_header
        IMPORTING
          r_salv_table   = go_salv
        CHANGING
          t_table        = gt_vbak
      ).
    CATCH cx_salv_msg.
  ENDTRY.

  SET HANDLER lcl_events=>on_link_click     FOR go_salv->get_event( ).
  SET HANDLER lcl_events=>on_double_click   FOR go_salv->get_event( ).
  SET HANDLER lcl_events=>on_added_function FOR go_salv->get_event( ).

  go_salv->get_display_settings( )->set_striped_pattern( abap_true ).
  go_salv->get_selections( )->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).
  go_salv->display( ).


ENDFORM.
