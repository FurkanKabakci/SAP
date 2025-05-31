*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1012
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1012.

*VALUES OF DATABASE TABLE
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000    EUR   DAVID.ALABA@TRIAL.COM
*2025000003 HARRY     KANE      TEACHER       2.500   EUR   HARRY.KANE@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350    EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000    EUR   ALEXANDER.ARNOLD@TRIAL.COM

DATA: gs_str   TYPE zfk_sap_tbl_01,
      gt_table TYPE TABLE OF zfk_sap_tbl_01.

SELECT * FROM zfk_sap_tbl_01 INTO TABLE gt_table. "We can get all values of table with *.

LOOP AT gt_table INTO gs_str.

  WRITE: gs_str-id,
         gs_str-name,
         gs_str-surname,
         gs_str-job,
         gs_str-salary,
         gs_str-currency,
         gs_str-email.

  SKIP.

ENDLOOP.

*OUTPUT
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000    EUR   DAVID.ALABA@TRIAL.COM
*2025000003 HARRY     KANE      TEACHER       2.500   EUR   HARRY.KANE@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350    EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000    EUR   ALEXANDER.ARNOLD@TRIAL.COM

ULINE.

SELECT * FROM zfk_sap_tbl_01 INTO TABLE gt_table UP TO 3 ROWS.

LOOP AT gt_table INTO gs_str.

  WRITE: gs_str-id,
         gs_str-name,
         gs_str-surname,
         gs_str-job,
         gs_str-salary,
         gs_str-currency,
         gs_str-email.

  SKIP.

ENDLOOP.

*OUTPUT
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000    EUR   DAVID.ALABA@TRIAL.COM
*2025000003 HARRY     KANE      TEACHER       2.500   EUR   HARRY.KANE@TRIAL.COM


ULINE.

SELECT id job salary FROM zfk_sap_tbl_01 INTO CORRESPONDING FIELDS OF TABLE gt_table WHERE id > 2025000003. " We can give conditions with WHERE. We can also write column names instead of *.

LOOP AT gt_table INTO gs_str.

  WRITE: gs_str-id,
         gs_str-job,
         gs_str-salary.

  SKIP.

ENDLOOP.

*OUTPUT
*2025000004 TAXI DRIVER   2350
*2025000005 ENGINEER      3000

ULINE.


SELECT SINGLE * FROM zfk_sap_tbl_01 INTO gs_str. "We can only get values of first line with SINGLE *, but we must use structure for line.

WRITE: gs_str-id,
       gs_str-name,
       gs_str-surname,
       gs_str-job,
       gs_str-salary,
       gs_str-currency,
       gs_str-email.

SKIP.
ULINE.

*OUTPUT
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM


SELECT SINGLE * FROM zfk_sap_tbl_01 INTO gs_str WHERE id = 2025000003. "We can only get values of one line with SINGLE * and WHERE, but we must use structure for line.

WRITE: gs_str-id,
       gs_str-name,
       gs_str-surname,
       gs_str-job,
       gs_str-salary,
       gs_str-currency,
       gs_str-email.

SKIP.
ULINE.

*OUTPUT
*2025000003 HARRY     KANE      TEACHER       2.500   EUR   HARRY.KANE@TRIAL.COM
