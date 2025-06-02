*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1014
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1014.

*VALUES OF DATABASE TABLE
*    ID     NAME      SURNAME    JOB          SALARY  CURRENCY        EMAIL
*  numc10   char40    char40    char40        int2     char3         char40
*2025000001 HANS      BÖCKLER   DOCTOR        3.500     EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000      EUR   DAVID.ALABA@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350      EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000      EUR   ALEXANDER.ARNOLD@TRIAL.COM
*2025000006 COLE      PALMER    LAWYER        2.500     EUR   COLE.PALMER@TRIAL.COM

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001 NO INTERVALS.
PARAMETERS: p_id     TYPE n LENGTH 10 OBLIGATORY,
            p_name   TYPE c LENGTH 40,
            p_sname  TYPE c LENGTH 40,
            p_job    TYPE c LENGTH 40,
            p_salary TYPE i,
            p_crr    TYPE c LENGTH 3,
            p_email  TYPE c LENGTH 40.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-002 NO INTERVALS.
PARAMETERS: p_create RADIOBUTTON GROUP a,
            p_read   RADIOBUTTON GROUP a,
            p_update RADIOBUTTON GROUP a,
            p_delete RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK a2.

DATA: gs_str   TYPE zfk_sap_tbl_01,
      gt_table TYPE TABLE OF zfk_sap_tbl_01.


START-OF-SELECTION.

  IF p_create = abap_true.

    SELECT SINGLE * FROM zfk_sap_tbl_01 INTO gs_str WHERE id = p_id.

    IF sy-subrc IS INITIAL.

      MESSAGE | { p_id } ID number has already been registered. | TYPE 'S' DISPLAY LIKE 'E'.

    ELSE.

      gs_str-id        = p_id.

      IF p_name IS NOT INITIAL AND p_sname IS NOT INITIAL AND p_job IS NOT INITIAL AND p_salary IS NOT INITIAL AND p_crr IS NOT INITIAL AND p_email IS NOT INITIAL.
        gs_str-name      = p_name.
        gs_str-surname   = p_sname.
        gs_str-job       = p_job.
        gs_str-salary    = p_salary.
        gs_str-currency  = p_crr.
        gs_str-email     = p_email.

        INSERT zfk_sap_tbl_01 FROM gs_str.

        MESSAGE | Successfully registered with { p_id } ID number. | TYPE 'I'.

      ELSE.

        MESSAGE: 'All fields must be filled.' TYPE 'S' DISPLAY LIKE 'E'.

      ENDIF.

    ENDIF.

  ELSEIF p_read = abap_true.

    SELECT SINGLE * FROM zfk_sap_tbl_01 INTO gs_str WHERE id = p_id.

    IF sy-subrc IS INITIAL.

      WRITE:  gs_str-id,
              gs_str-name,
              gs_str-surname,
              gs_str-job,
              gs_str-salary,
              gs_str-currency,
              gs_str-email.

    ELSE.

      MESSAGE | { p_id } ID number not found. | TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ELSEIF p_update = abap_true.

    SELECT SINGLE * FROM zfk_sap_tbl_01 INTO gs_str WHERE id = p_id.

    IF sy-subrc IS INITIAL.

      IF p_name IS NOT INITIAL.

        UPDATE zfk_sap_tbl_01 SET name = p_name WHERE id = p_id.

      ENDIF.
      IF p_sname IS NOT INITIAL.

        UPDATE zfk_sap_tbl_01 SET surname = p_sname WHERE id = p_id.

      ENDIF.
      IF p_job IS NOT INITIAL.

        UPDATE zfk_sap_tbl_01 SET job = p_job WHERE id = p_id.

      ENDIF.
      IF p_salary IS NOT INITIAL.

        UPDATE zfk_sap_tbl_01 SET salary = p_salary WHERE id = p_id.

      ENDIF.
      IF p_crr IS NOT INITIAL.

        UPDATE zfk_sap_tbl_01 SET currency = p_crr WHERE id = p_id.

      ENDIF.
      IF p_email IS NOT INITIAL.

        UPDATE zfk_sap_tbl_01 SET email = p_email WHERE id = p_id.

      ENDIF.

      MESSAGE | The record with ID { p_id } has been successfully updated. | TYPE 'S'.

    ELSE.

      MESSAGE | { p_id } ID number not found. | TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ELSEIF p_delete = abap_true.

    SELECT SINGLE * FROM zfk_sap_tbl_01 INTO gs_str WHERE id = p_id.

    IF sy-subrc IS INITIAL.

      DELETE FROM zfk_sap_tbl_01 WHERE id = p_id.

      MESSAGE | The record with ID { p_id } has been deleted. | TYPE 'S'.

    ELSE.

      MESSAGE | { p_id } ID number not found. | TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ENDIF.
