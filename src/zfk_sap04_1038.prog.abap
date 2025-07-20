*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1038
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1038.


*A deep structure is a structure that contains other structures or internal tables within it.
*In other words, its fields are not only made up of simple data types but can also consist of
*other structures or table types. Such structures can be created manually like this, or they
*can also be created via transaction SE11.

TYPES: BEGIN OF gty_str_info,
         name        TYPE c LENGTH 40,
         surname     TYPE c LENGTH 40,
         birth_place TYPE c LENGTH 40,
         birthday    TYPE sy-datum,
       END OF gty_str_info.

TYPES: BEGIN OF gty_str_school_info,
         school_name TYPE c LENGTH 40,
         department  TYPE c LENGTH 40,
         class       TYPE c LENGTH 40,
         school_nu   TYPE c LENGTH 6,
       END OF gty_str_school_info.

TYPES: BEGIN OF gty_str,
         id_nu       TYPE c LENGTH 6,
         id_info     TYPE gty_str_info, "As you can see here, when defining a structure,
                                        "we create a deep structure by assigning the TYPE of one of its fields to the structure we previously created.
         school_info TYPE gty_str_school_info,
         adress      TYPE c LENGTH 80,
       END OF gty_str.


DATA: gs_str TYPE gty_str,
      gt_tbl TYPE TABLE OF gty_str.

gs_str-id_nu                   = 012345.
gs_str-id_info-name            = 'Harry'.
gs_str-id_info-surname         = 'Kane'.
gs_str-id_info-birth_place     = 'London'.
gs_str-id_info-birthday        = '19900101'.
gs_str-school_info-school_name = 'Cambridge'.
gs_str-school_info-department  = 'Economy'.
gs_str-school_info-class       = '72B'.
gs_str-school_info-school_nu   = 454567.
gs_str-adress                  = 'London'.


APPEND gs_str TO gt_tbl.

WRITE: 'Id Nu:', gs_str-id_nu,
       / 'Identification Info',
       / 'Name:', gs_str-id_info-name,
       / 'Surname:', gs_str-id_info-surname,
       / 'Place of Birth:', gs_str-id_info-birth_place,
       / 'Birthday:', gs_str-id_info-birthday,
       / 'School Info',
       / 'School Name:', gs_str-school_info-school_name,
       / 'Department:', gs_str-school_info-department,
       / 'Class:', gs_str-school_info-class,
       / 'School Nu:', gs_str-school_info-school_nu,
       / 'Adress Info',
       / 'Adress:', gs_str-adress.
