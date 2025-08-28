*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1048
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1048.

*In SAP ABAP, SNRO (Number Range Objects) is used to generate sequential and unique numbers.
*A number range object (e.g., ZORDERS) with defined intervals is created in SNRO, and in ABAP
*the NUMBER_GET_NEXT function module is called to fetch the next available number. This ensures
*no ID conflicts occur even when multiple users run the report simultaneously. Typical use cases
*include order numbers, invoice numbers, IDs, and log entries. In this example, the STRAVELAG
*training table copy is used, where the ID field is generated sequentially via SNRO.

DATA: gs_str TYPE zfk_stravelag.

START-OF-SELECTION.

SELECT * FROM stravelag INTO TABLE @DATA(gt_tbl).

LOOP AT gt_tbl INTO DATA(gs_tbl).
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZFK_SNRO_1'
    IMPORTING
      number                  = gs_str-id "We get the ID automatically from the function.
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  gs_str-agencynum  = gs_tbl-agencynum.
  gs_str-name       = gs_tbl-name.
  gs_str-street     = gs_tbl-street.
  gs_str-postbox    = gs_tbl-postbox.
  gs_str-postcode   = gs_tbl-postcode.
  gs_str-city       = gs_tbl-city.
  gs_str-country    = gs_tbl-country.
  gs_str-region     = gs_tbl-region.
  gs_str-telephone  = gs_tbl-telephone.
  gs_str-url        = gs_tbl-url.
  gs_str-langu      = gs_tbl-langu.
  gs_str-currency   = gs_tbl-currency.

  INSERT zfk_stravelag FROM gs_str.
ENDLOOP.
