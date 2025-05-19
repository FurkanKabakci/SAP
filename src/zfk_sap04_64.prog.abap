*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_64
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_64.


"Alternatif.

*DATA: BEGIN OF gs_structure,
*        id      TYPE n LENGTH 8,
*        name    TYPE c LENGTH 40,
*        surname TYPE c LENGTH 40,
*        job     TYPE c LENGTH 20,
*        salary  TYPE i,
*        curr    TYPE c LENGTH 3,
*        gsm     TYPE string,
*        e_mail  TYPE string,
*      END OF gs_structure.

"En saglikli yöntem.
TYPES: BEGIN OF gty_structure,
         id      TYPE n LENGTH 8,
         name    TYPE c LENGTH 40,
         surname TYPE c LENGTH 40,
         job     TYPE c LENGTH 20,
         salary  TYPE i,
         curr    TYPE c LENGTH 3,
         gsm     TYPE string,
         e_mail  TYPE string,
       END OF gty_structure.

DATA: gs_structure TYPE gty_structure,
      gt_table     TYPE TABLE OF gty_structure.

gs_structure-id      = 12345.
gs_structure-name    = 'Mehmet'.
gs_structure-surname = 'Yilmaz'.
gs_structure-job     = 'Teacher'.
gs_structure-salary  = 3000.
gs_structure-curr    = 'EUR'.
gs_structure-gsm     = '+90123456789'.
gs_structure-e_mail  = 'mehmetyilmaz@gmail.com'.

APPEND gs_structure TO gt_table.
CLEAR: gs_structure.

gs_structure-id      = 12346.
gs_structure-name    = 'Murat'.
gs_structure-surname = 'Öztürk'.
gs_structure-job     = 'Engineer'.
gs_structure-salary  = 4000.
gs_structure-curr    = 'EUR'.
gs_structure-gsm     = '+90126543789'.
gs_structure-e_mail  = 'muratozturk@gmail.com'.

APPEND gs_structure TO gt_table.
CLEAR: gs_structure.

gs_structure-id      = 12347.
gs_structure-name    = 'Meral'.
gs_structure-surname = 'Öztürk'.
gs_structure-job     = 'Engineer'.
gs_structure-salary  = 4000.
gs_structure-curr    = 'EUR'.
gs_structure-gsm     = '+90126543789'.
gs_structure-e_mail  = 'muratozturk@gmail.com'.

APPEND gs_structure TO gt_table.
CLEAR: gs_structure.

gs_structure-id      = 12348.
gs_structure-name    = 'Mert'.
gs_structure-surname = 'Öztürk'.
gs_structure-job     = 'Engineer'.
gs_structure-salary  = 4000.
gs_structure-curr    = 'EUR'.
gs_structure-gsm     = '+90126543789'.
gs_structure-e_mail  = 'muratozturk@gmail.com'.

APPEND gs_structure TO gt_table.
CLEAR: gs_structure.

gs_structure-id      = 12349.
gs_structure-name    = 'Merve'.
gs_structure-surname = 'Öztürk'.
gs_structure-job     = 'Engineer'.
gs_structure-salary  = 4000.
gs_structure-curr    = 'EUR'.
gs_structure-gsm     = '+90126543789'.
gs_structure-e_mail  = 'muratozturk@gmail.com'.

APPEND gs_structure TO gt_table.
CLEAR: gs_structure.

LOOP AT gt_table INTO gs_structure.

  WRITE: gs_structure-id,
         gs_structure-name.

  SKIP.

ENDLOOP.

BREAK-POINT.
