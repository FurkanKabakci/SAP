*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1049
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1049.

*Create a new report. Prompt the user to enter 1 CARRID and 1 CONNID.
*Ensure that these parameters appear as dropdown lists on the report screen.
*Define a parameter for each cell of the SPFLI table on the screen.
*Arrange these parameters side by side, make them read-only, and invisible
*if there is no data. Based on the user’s selection, read the corresponding row
*from the SPFLI table and populate the cell parameters on the screen with
*the retrieved row data, making them visible.


SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.

*The SELECTION-SCREEN BEGIN OF LINE statement is used to display multiple parameters or
*fields side by side on the same line of the selection screen. Parameters placed inside
*this block are ended with END OF LINE, making the screen more compact and user-friendly.

SELECTION-SCREEN COMMENT 1(6) TEXT-002.
*The values inside the parentheses of the SELECTION-SCREEN BEGIN OF LINE statement determine
*the position and alignment of the fields within the line. This allows precise control over
*the placement of each field on the screen.
SELECTION-SCREEN POSITION 8.
PARAMETERS: p_carrid TYPE s_carr_id AS LISTBOX VISIBLE LENGTH 5. "AS LISTBOX is for dropdown.

SELECTION-SCREEN COMMENT 14(8) TEXT-003.
SELECTION-SCREEN POSITION 22.
PARAMETERS: p_connid TYPE s_conn_id AS LISTBOX VISIBLE LENGTH 10.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(12) TEXT-004.
SELECTION-SCREEN POSITION 13.
PARAMETERS: p_cntrfr TYPE land1.

SELECTION-SCREEN COMMENT 19(9) TEXT-005.
SELECTION-SCREEN POSITION 28.
PARAMETERS: p_ctyfrm TYPE s_from_cit.

SELECTION-SCREEN COMMENT 50(12) TEXT-006.
SELECTION-SCREEN POSITION 62.
PARAMETERS: p_arpfrm TYPE s_fromairp.

SELECTION-SCREEN COMMENT 66(10) TEXT-007.
SELECTION-SCREEN POSITION 76.
PARAMETERS: p_cntrto TYPE land1.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a2.

INITIALIZATION.

*In SAP ABAP, the INITIALIZATION event is triggered before the report is executed,
*just before the selection screen is displayed. It is used to assign default values
*to parameters and selection criteria or to populate dropdown lists. When the user sees
*the selection screen, the predefined initial values are displayed automatically.

  PERFORM dropdown.

AT SELECTION-SCREEN.

*This event is triggered after the user has entered values on the selection screen and pressed
*the “Execute” button. It is used to validate or check user input, for example, preventing
*empty values or enforcing a value range.

  SELECT SINGLE * FROM spfli INTO @DATA(ls_spfli) WHERE carrid = @p_carrid AND connid = @p_connid.

  p_cntrfr = ls_spfli-countryfr.
  p_ctyfrm = ls_spfli-cityfrom.
  p_arpfrm = ls_spfli-airpfrom.
  p_cntrto = ls_spfli-countryto.

AT SELECTION-SCREEN OUTPUT.

*This event is triggered just before the selection screen is displayed. It is used to dynamically
*modify the appearance or attributes of screen fields, such as showing/hiding fields or changing
*colors and input modes.

  IF p_carrid IS INITIAL OR p_connid IS INITIAL.
    LOOP AT SCREEN. "The SCREEN-NAME values of the selection screen fields can be obtained using the SAP debugger.
      IF screen-name = '%C004012_1000' OR screen-name = 'P_CNTRFR' OR screen-name = '%C005015_1000' OR screen-name = 'P_CTYFRM' OR screen-name = '%C006018_1000' OR screen-name = 'P_ARPFRM' OR screen-name = '%C007021_1000' OR  screen-name = 'P_CNTRTO'.
        screen-input  = '0'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

FORM dropdown .

*A dropdown is a field on the selection screen that provides the user with predefined options.
*The VRM_SET_VALUES function is used to dynamically set these dropdown values within the program,
*allowing the user to select only from the defined options.

  DATA: lv_fname  TYPE vrm_id,
        lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  lv_fname = 'p_carrid'.
  SELECT carrid FROM spfli INTO TABLE @DATA(lt_carrid).
    LOOP AT lt_carrid INTO DATA(ls_carrid).

      ls_value-key = ls_carrid-carrid.
      ls_value-text = ls_carrid-carrid.

      APPEND ls_value TO lt_values.

    ENDLOOP.

    SORT lt_values.
    DELETE ADJACENT DUPLICATES FROM lt_values.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = lv_fname
        values          = lt_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    CLEAR: lv_fname, lt_values, ls_value.

    lv_fname = 'p_connid'.
    SELECT connid FROM spfli INTO TABLE @DATA(lt_connid).
      LOOP AT lt_connid INTO DATA(ls_connid).

        ls_value-key = ls_connid-connid.
        ls_value-text = ls_connid-connid.

        APPEND ls_value TO lt_values.

      ENDLOOP.

      SORT lt_values.
      DELETE ADJACENT DUPLICATES FROM lt_values.

      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = lv_fname
          values          = lt_values
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      CLEAR: lv_fname, lt_values, ls_value.


ENDFORM.
