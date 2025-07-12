*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1029
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1029.

*ALV (ABAP List Viewer) is a standard SAP tool used in ABAP programs to display data from
*      internal tables or database tables in a grid (table) format. It facilitates user interaction
*      and improves flexibility and readability in reporting.
*
*There are two methods to create a Simple ALV: using the REUSE_ALV_GRID_DISPLAY function module and
*      the LVC (List Viewer Control) functions. When using the LVC structure, the LVC_FIELDCATALOG_MERGE
*            function module is typically used to generate the field catalog.
*
*An ALV is typically built in four steps:
* 1.  Data Retrieval: The required data is fetched from the database or internal tables.
* 2.  Field Catalog Creation: The properties of the columns to be displayed in the ALV are defined
*  (LVC_FIELDCATALOG_MERGE is used for LVC-based ALVs).
* 3.  Layout Configuration: Visual adjustments and user-friendly settings are applied.
* 4.  Displaying the ALV: The prepared ALV grid is presented to the user.


*In REUSE_ALV_GRID_DISPLAY (classic ALV), a selection (checkbox) column must be manually added at
*the beginning of the table to allow users to select rows. This is one of the main differences between
*classic ALV and LVC (Object-Oriented ALV). In LVC-based ALV, row selection is supported by default and
*no additional column is needed.
*
*To add a selection column in classic ALV, define it as follows:

TYPES: BEGIN OF gty_table,
         box.
    INCLUDE STRUCTURE sbook.
TYPES: END OF gty_table.

DATA: gs_str      TYPE gty_table,
      gt_table    TYPE TABLE OF gty_table,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
*      In the first method, we use a table of type slis_t_fieldcat_alv to define the field catalog.
      gs_layout   TYPE slis_layout_alv.
*      “For the layout, we also use the data type slis_layout_alv.

*1.	Data Retrieval:

SELECT * FROM sbook INTO CORRESPONDING FIELDS OF TABLE gt_table.

* 2.  Field Catalog Creation:

CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
    i_structure_name       = 'sbook'
    i_bypassing_buffer     = 'X'
  CHANGING
    ct_fieldcat            = gt_fieldcat
  EXCEPTIONS
    inconsistent_interface = 1
    program_error          = 2
    OTHERS                 = 3.
IF sy-subrc IS NOT INITIAL.
  EXIT.
ENDIF.

* 3.  Layout Configuration:

*You can further expand these options.
gs_layout-zebra             = abap_true.
gs_layout-colwidth_optimize = abap_true.
gs_layout-box_fieldname     = 'BOX'.      "We create the column we add at the beginning by assigning it a name.


*4.	Displaying the ALV:

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program = sy-repid
    is_layout          = gs_layout
    it_fieldcat        = gt_fieldcat
  TABLES
    t_outtab           = gt_table
  EXCEPTIONS
    program_error      = 1
    OTHERS             = 2.
