*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1037
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1037.

PARAMETERS: p_curr TYPE s_currcode.


CLASS lcl_get_data DEFINITION.

  PUBLIC SECTION.

    DATA:   mt_scarr TYPE zfk_tt_scarr,
            mt_table TYPE zfk_tt_sbook_trial_1.

    METHODS:  get_scarr   IMPORTING iv_curr TYPE s_currcode,

              get_sbook   ,

              display_alv .

ENDCLASS.

CLASS lcl_get_data IMPLEMENTATION.

  METHOD get_scarr.
    SELECT * FROM scarr INTO TABLE mt_scarr WHERE currcode = iv_curr.
  ENDMETHOD.

  METHOD get_sbook.

    SELECT * FROM sbook INTO CORRESPONDING FIELDS OF TABLE mt_table UP TO 250 ROWS FOR ALL ENTRIES IN  mt_scarr WHERE carrid = mt_scarr-carrid.

  ENDMETHOD.

  METHOD display_alv.
    DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
          ls_layout   TYPE slis_layout_alv.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZFK_SBOOK_S_TRIAL_1'
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    ls_layout-zebra              = abap_true.
    ls_layout-colwidth_optimize  = abap_true.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = ls_layout
        it_fieldcat        = lt_fieldcat
      TABLES
        t_outtab           = mt_table
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

DATA: go_get_data TYPE REF TO lcl_get_data,
      gt_table    TYPE zfk_tt_sbook_trial_1,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.


START-OF-SELECTION.

  CREATE OBJECT go_get_data.

  go_get_data->get_scarr(
        EXPORTING
          iv_curr = p_curr ).

  go_get_data->get_sbook( ).

  go_get_data->display_alv( ).
