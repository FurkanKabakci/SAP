*&---------------------------------------------------------------------*
*& Report ZFK_SAP_INTERNAL_TABLES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFK_SAP_INTERNAL_TABLES.

TYPES: BEGIN OF gty_structure,
         id         TYPE n LENGTH 6,
         name       TYPE c LENGTH 10,
         surname    TYPE c LENGTH 10,
         profession TYPE c LENGTH 10,
         salary     TYPE i,
         currency   TYPE c LENGTH 3,
         tel_nu     TYPE n LENGTH 15,
         email      TYPE c LENGTH 25,
       END OF gty_structure.

DATA: gs_structure TYPE gty_structure,
      gt_table     TYPE TABLE OF gty_structure,
      gv_number    TYPE n LENGTH 2,
      gv_id        TYPE n LENGTH 6,
      gv_salary    TYPE i.


SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001 NO INTERVALS.
PARAMETERS: p_id    TYPE n LENGTH 6,
            p_name  TYPE c LENGTH 10,
            p_sname TYPE c LENGTH 10,
            p_prfsn TYPE c LENGTH 10,
            p_slary TYPE i,
            p_crr   TYPE c LENGTH 3,
            p_tel   TYPE n LENGTH 15,
            p_email TYPE c LENGTH 25.
SELECTION-SCREEN END OF BLOCK a1.

START-OF-SELECTION.

  "Create Processes

  "We used simply DO and Parameters to create table.

  gv_id     = p_id.
  gv_salary = p_slary.

  DO 5 TIMES.

    gs_structure-id           = gv_id.
    gs_structure-name         = p_name.
    gs_structure-surname      = p_sname.
    gs_structure-profession   = p_prfsn.
    gs_structure-salary       = gv_salary.
    gs_structure-currency     = p_crr.
    gs_structure-tel_nu       = p_tel.
    gs_structure-email        = p_email.

    APPEND gs_structure TO gt_table.
    CLEAR: gs_structure.

    ADD 1 TO gv_id.
    gv_salary = gv_salary + 500.

  ENDDO.

  "Also we can manually add structure to table like comments.

*gs_structure-id = 100001.
*gs_structure-name = 'Michael'.
*gs_structure-surname = 'Jonathan'.
*gs_structure-profession = 'Teacher'.
*gs_structure-salary = 3000.
*gs_structure-currency = 'EUR'.
*gs_structure-tel_nu = 123476890.
*gs_structure-email = 'trial@gmail.com'.
*
*
*  APPEND gs_structure TO gt_table.
*  CLEAR: gs_structure.


  DESCRIBE TABLE gt_table LINES gv_number. "We can find number of lines with this comment.

  WRITE: 'Number of lines:', gv_number.
  SKIP.

  LOOP AT gt_table INTO gs_structure. "We can print data to the screen with loop. We can also use FROM and Where commands with LOOP.

    WRITE: gs_structure-id,
           gs_structure-name,
           gs_structure-surname,
           gs_structure-profession,
           gs_structure-salary,
           gs_structure-currency,
           gs_structure-tel_nu,
           gs_structure-email.

    SKIP.
  ENDLOOP.

  ULINE.

  WRITE: 'LOOP with FROM... TO...'.
  SKIP.

  LOOP AT gt_table INTO gs_structure FROM 2 TO 4 . "We can also use FROM and Where commands with LOOP.

    WRITE: gs_structure-id,
           gs_structure-name,
           gs_structure-surname,
           gs_structure-profession,
           gs_structure-salary,
           gs_structure-currency,
           gs_structure-tel_nu,
           gs_structure-email.

    SKIP.
  ENDLOOP.

  ULINE.

  WRITE: 'LOOP with WHERE...'.
  SKIP.

  LOOP AT gt_table INTO gs_structure WHERE id BETWEEN 100003 AND 100004 . "We can also use FROM and Where commands with LOOP.

    WRITE: gs_structure-id,
           gs_structure-name,
           gs_structure-surname,
           gs_structure-profession,
           gs_structure-salary,
           gs_structure-currency,
           gs_structure-tel_nu,
           gs_structure-email.

    SKIP.
  ENDLOOP.

  ULINE.

  SORT gt_table BY id DESCENDING. " Default value is ASCENDING.

  WRITE: 'Sorted Table by id field (Descending)'.
  SKIP.

  LOOP AT gt_table INTO gs_structure.

    WRITE: / gs_structure-id,
             gs_structure-name,
             gs_structure-surname,
             gs_structure-profession,
             gs_structure-salary,
             gs_structure-currency,
             gs_structure-tel_nu,
             gs_structure-email.

    SKIP.
  ENDLOOP.

  ULINE.

  SORT gt_table BY salary.
  WRITE: 'Sorted Table by salary field (Ascending)'.
  SKIP.

  LOOP AT gt_table INTO gs_structure.

    WRITE: / gs_structure-id,
             gs_structure-name,
             gs_structure-surname,
             gs_structure-profession,
             gs_structure-salary,
             gs_structure-currency,
             gs_structure-tel_nu,
             gs_structure-email.
    SKIP.
  ENDLOOP.

  ULINE.

  "READ Process

  READ TABLE gt_table INTO gs_structure WITH KEY id = 100003. "We can read specific line with READ TABLE ... INTO... , but we must always check sy-subrc value.

  WRITE: 'With Read Table... INTO.... WITH KEY....'.
  SKIP.

  IF sy-subrc IS INITIAL.
    WRITE: 'Satir nu:', sy-tabix,
           gs_structure-id,
           gs_structure-name,
           gs_structure-surname,
           gs_structure-profession,
           gs_structure-salary,
           gs_structure-currency,
           gs_structure-tel_nu,
           gs_structure-email.

  ENDIF.

  ULINE.

  READ TABLE gt_table INTO gs_structure INDEX 3. "We can read specific line with READ TABLE ... INTO... , but we must always check sy-subrc value.

  WRITE: 'With Read Table Command...'.
  SKIP.

  IF sy-subrc IS INITIAL.
    WRITE: 'Satir nu:', sy-tabix,
           gs_structure-id,
           gs_structure-name,
           gs_structure-surname,
           gs_structure-profession,
           gs_structure-salary,
           gs_structure-currency,
           gs_structure-tel_nu,
           gs_structure-email.

  ENDIF.

  ULINE.


  "UPDATE Processes

  gs_structure-salary = gs_structure-salary + 1000.

  MODIFY gt_table FROM gs_structure INDEX 3. "We can update values or lines in the table with MODIFY ... FROM....
  CLEAR: gs_structure.

  WRITE: 'After salary update'.
  SKIP.

  READ TABLE gt_table INTO gs_structure INDEX 3.

  IF sy-subrc IS INITIAL.
    WRITE: 'Satir nu:', sy-tabix,
           gs_structure-id,
           gs_structure-name,
           gs_structure-surname,
           gs_structure-profession,
           gs_structure-salary,
           gs_structure-currency,
           gs_structure-tel_nu,
           gs_structure-email.
  ENDIF.

  ULINE.

  WRITE: 'After salary update with Transporting'.
  SKIP.

  gs_structure-salary = 4000.
  gs_structure-email = 'trial@company.com'.

  MODIFY gt_table FROM gs_structure TRANSPORTING salary email WHERE id = 100003.
  CLEAR: gs_structure.

  READ TABLE gt_table INTO gs_structure WITH KEY id = 100003. "We can read specific line with READ TABLE ... INTO... , but we must always check sy-subrc value.

  IF sy-subrc IS INITIAL.
    WRITE: 'Satir nu:', sy-tabix,
           gs_structure-id,
           gs_structure-name,
           gs_structure-surname,
           gs_structure-profession,
           gs_structure-salary,
           gs_structure-currency,
           gs_structure-tel_nu,
           gs_structure-email.

  ENDIF.

  ULINE.


  "DELETE Processes


  DELETE gt_table INDEX 3. "We can delete lines in the table with DELETE command.

  WRITE: 'After deleting the line...'.
  SKIP.

  DESCRIBE TABLE gt_table LINES gv_number.

  WRITE: 'Number of lines:', gv_number.
  SKIP.

  LOOP AT gt_table INTO gs_structure.

    WRITE: / gs_structure-id,
             gs_structure-name,
             gs_structure-surname,
             gs_structure-profession,
             gs_structure-salary,
             gs_structure-currency,
             gs_structure-tel_nu,
             gs_structure-email.
    SKIP.
  ENDLOOP.

  ULINE.
