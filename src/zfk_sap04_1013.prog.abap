*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1013
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1013.

*VALUES OF DATABASE TABLE
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000    EUR   DAVID.ALABA@TRIAL.COM
*2025000003 HARRY     KANE      TEACHER       2.500   EUR   HARRY.KANE@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350    EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000    EUR   ALEXANDER.ARNOLD@TRIAL.COM

DATA: gs_str   TYPE zfk_sap_tbl_01,
      gt_table TYPE TABLE OF zfk_sap_tbl_01.

"INSERT COMMAND

gs_str-id        = 2025000006.
gs_str-name      = 'COLE'.
gs_str-surname   = 'PALMER'.
gs_str-job       = 'LAWYER'.
gs_str-salary    = 3500.
gs_str-currency  = 'EUR'.
gs_str-email     = 'COLE.PALMER@TRIAL.COM'.

INSERT zfk_sap_tbl_01 FROM gs_str.

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
  CLEAR: gs_str.
ENDLOOP.

ULINE.

*OUTPUT
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000    EUR   DAVID.ALABA@TRIAL.COM
*2025000003 HARRY     KANE      TEACHER       2.500   EUR   HARRY.KANE@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350    EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000    EUR   ALEXANDER.ARNOLD@TRIAL.COM
*2025000006 COLE      PALMER    LAWYER        3500    EUR   COLE.PALMER@TRIAL.COM

"UPDATE COMMAND

UPDATE zfk_sap_tbl_01 SET surname  = 'ADAMS'
                          salary   = 5750
                          currency = 'USD'
                          email    = 'HARRY.ADAMS@TRIAL.COM'
                          WHERE id = 2025000003.

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
  CLEAR: gs_str.
ENDLOOP.

ULINE.

*OUTPUT
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000    EUR   DAVID.ALABA@TRIAL.COM
*2025000003 HARRY     ADAMS     TEACHER       5750    USD   HARRY.KANE@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350    EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000    EUR   ALEXANDER.ARNOLD@TRIAL.COM
*2025000006 COLE      PALMER    LAWYER        3500    EUR   COLE.PALMER@TRIAL.COM

"DELETE COMMAND

DELETE FROM zfk_sap_tbl_01 WHERE id = 2025000003.

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
  CLEAR: gs_str.
ENDLOOP.

ULINE.

*OUTPUT
*2025000001 HANS      BÖCKLER   DOCTOR        3.500   EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000    EUR   DAVID.ALABA@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350    EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000    EUR   ALEXANDER.ARNOLD@TRIAL.COM
*2025000006 COLE      PALMER    LAWYER        3500    EUR   COLE.PALMER@TRIAL.COM
