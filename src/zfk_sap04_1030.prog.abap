*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1030
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1030.

*We can select a row in the ALV table and bring it to the foreground in a pop-up window for editing.
*Additionally, we can create our own custom interface buttons.

TYPES: BEGIN OF gty_table,
         box.
    INCLUDE STRUCTURE zfk_sap04_spfli.
TYPES: END OF gty_table.

DATA: gt_table    TYPE TABLE OF gty_table,
      gt_selected TYPE TABLE OF gty_table,
      gs_selected TYPE gty_table,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gv_answer.

START-OF-SELECTION.

  SELECT * FROM zfk_sap04_spfli INTO CORRESPONDING FIELDS OF TABLE gt_table.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZFK_SAP04_SPFLI'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  gs_layout-zebra = abap_true.
  gs_layout-colwidth_optimize = abap_true.
  gs_layout-box_fieldname = 'BOX'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_1030' "We can enable the option to create our own buttons by using PF_STATUS.
      i_callback_user_command  = 'UC_1030' "With USER_COMMAND, we can retrieve the data that the user clicked on with the mouse.
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
    TABLES
      t_outtab                 = gt_table
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* Here, we can either define our own PF_STATUS or copy an existing one provided by SAP via transaction SE41 and make additions to it.
*To enter the editing area, we need to double-click on the STATUS_1030 text.

FORM pf_1030 USING lt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STATUS_1030'.
ENDFORM.

*In the FORM we define here, we implement the logic that responds to the actions performed by the user in the interface.
*While lv_ucomm tells us which user command was triggered, ls_selfield contains the data related to the selected row or field.

FORM uc_1030 USING lv_ucomm     TYPE sy-ucomm
                   ls_selfield  TYPE slis_selfield.

  CASE lv_ucomm.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.
    WHEN '&IC1'.
      IF ls_selfield-fieldname = 'CONNID'.
        SELECT * FROM zfk_sap04_spfli INTO CORRESPONDING FIELDS OF TABLE gt_selected WHERE connid = ls_selfield-value.

*        The editability of rows and the ability to input data into fields are controlled via the field catalog settings.

        LOOP AT gt_fieldcat INTO gs_fieldcat.
          gs_fieldcat-edit = abap_true.
          gs_fieldcat-input = abap_true.

          MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix.
        ENDLOOP.

*        We use the function module REUSE_ALV_POPUP_TO_SELECT to bring the selected row into a pop-up for further interaction or selection.

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            i_title               = 'Info'
            i_screen_start_column = 5
            i_screen_start_line   = 5
            i_screen_end_column   = 165
            i_screen_end_line     = 8
            i_tabname             = 'gt_selected'
            it_fieldcat           = gt_fieldcat
            i_callback_program    = sy-repid
          IMPORTING
            e_exit                = gv_answer
          TABLES
            t_outtab              = gt_selected
          EXCEPTIONS
            program_error         = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF gv_answer IS INITIAL. "When the user clicks “OK” in the pop-up, the return value is blank. For any other selection, such as “Cancel”, the function returns 'X'.
          READ TABLE gt_selected INTO gs_selected INDEX 1.
          IF sy-subrc IS INITIAL.
            UPDATE zfk_sap04_spfli SET carrid     = gs_selected-carrid
                                       countryfr  = gs_selected-countryfr
                                       cityfrom   = gs_selected-cityfrom
                                       airpfrom   = gs_selected-airpfrom
                                       countryto  = gs_selected-countryto
                                       cityto     = gs_selected-cityto
                                       airpto     = gs_selected-airpto
                                       fltime     = gs_selected-fltime
                                       deptime    = gs_selected-deptime
                                       arrtime    = gs_selected-arrtime
                                       distance   = gs_selected-distance
                                       distid     = gs_selected-distid
                                       fltype     = gs_selected-fltype
                                       period     = gs_selected-period
                                    WHERE connid  = gs_selected-connid.

            SELECT * FROM zfk_sap04_spfli INTO CORRESPONDING FIELDS OF TABLE gt_table.

*            After an operation is executed, the ls_selfield-refresh flag can be set to refresh the displayed ALV table with updated data.

            ls_selfield-refresh = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.
