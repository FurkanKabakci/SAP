*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1014
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1021.

*SELECT-OPTIONS is used in ABAP to allow the user to input single values, ranges, multiple values, or exclusions on the selection screen.

*VALUES OF DATABASE TABLE
*    ID     NAME      SURNAME    JOB          SALARY  CURRENCY        EMAIL
*  numc10   char40    char40    char40        int2     char3         char40
*2025000001 HANS      BÖCKLER   DOCTOR        3.500     EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000      EUR   DAVID.ALABA@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350      EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000      EUR   ALEXANDER.ARNOLD@TRIAL.COM
*2025000006 COLE      PALMER    LAWYER        2.500     EUR   COLE.PALMER@TRIAL.COM

DATA: gs_str TYPE zfk_sap_tbl_01,
      gt_tbl TYPE TABLE OF zfk_sap_tbl_01.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: so_id FOR gs_str-id.

SELECTION-SCREEN END OF BLOCK a1.


START-OF-SELECTION.

  SELECT * FROM zfk_sap_tbl_01 INTO TABLE gt_tbl WHERE id IN so_id.

  LOOP AT gt_tbl INTO gs_str.

    WRITE: gs_str-id,
           gs_str-name,
           gs_str-surname,
           gs_str-job,
           gs_str-salary,
           gs_str-currency,
           gs_str-email.

  ENDLOOP.

*If we enter the range 2025000002 - 2025000005, we will get the result as follows:
*    ID     NAME      SURNAME    JOB          SALARY  CURRENCY        EMAIL
*  numc10   char40    char40    char40        int2     char3         char40
*2025000002 DAVID     ALABA     DEVELOPER     4000      EUR   DAVID.ALABA@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350      EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000      EUR   ALEXANDER.ARNOLD@TRIAL.COM
