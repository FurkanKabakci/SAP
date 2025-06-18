*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1022
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1022.

*SELECT-OPTIONS is used in ABAP to allow the user to input single values, ranges, multiple values, or exclusions on the selection screen.

*VALUES OF DATABASE TABLE
*    ID     NAME      SURNAME    JOB          SALARY  CURRENCY        EMAIL
*  numc10   char40    char40    char40        int2     char3         char40
*2025000001 HANS      BÖCKLER   DOCTOR        3.500     EUR   HANS.BÖCKLER@TRIAL.COM
*2025000002 DAVID     ALABA     DEVELOPER     4000      EUR   DAVID.ALABA@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350      EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000      EUR   ALEXANDER.ARNOLD@TRIAL.COM
*2025000006 COLE      PALMER    LAWYER        2.500     EUR   COLE.PALMER@TRIAL.COM



SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001 NO INTERVALS.

PARAMETERS: p_1   TYPE n LENGTH 10,      " same TYPE for id
            p_2   TYPE n LENGTH 10,      " same TYPE for id
            p_inc RADIOBUTTON GROUP a,   " inclusive
            p_exc RADIOBUTTON GROUP a,   " exclusive
            p_eq  RADIOBUTTON GROUP b,   " equal
            p_bt  RADIOBUTTON GROUP b.   " between

SELECTION-SCREEN END OF BLOCK a1.

DATA: gt_tbl TYPE TABLE OF zfk_sap_tbl_01,
      gs_str TYPE zfk_sap_tbl_01.

*We can manually create the structure of the select-options table that the system generates automatically.
*In a report where we use select-options, we can also verify this structure through the debugger screen.
*The important point is that the low and high values must have the same TYPE as our key values.

TYPES: BEGIN OF gty_str_id,
         sign   TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low    TYPE n LENGTH 10, " same TYPE with zfk_sap_tbl_01-id
         high   TYPE n LENGTH 10, " same TYPE with zfk_sap_tbl_01-id
       END OF gty_str_id.

*We use the TYPE RANGE OF statement to convert a manually created selection option structure
*into a select-options-compatible internal table, and we provide the type of the key field as the reference.

DATA: gs_selopt TYPE gty_str_id,
      gt_selopt TYPE RANGE OF zfk_sap_tbl_01-id.

START-OF-SELECTION.

  IF p_inc = abap_true.

    gs_selopt-sign = 'I'. " Inclusive

  ELSE.

    gs_selopt-sign = 'E'. " Exclusive

  ENDIF.

  IF p_eq = abap_true.

    gs_selopt-option = 'EQ'. " Equal

  ELSE.

    IF p_1 IS INITIAL OR p_2 IS INITIAL.

      MESSAGE 'IF you choose BT, you must fill all fields.' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ELSE.

      gs_selopt-option = 'BT'. " Between

    ENDIF.

  ENDIF.

  gs_selopt-low   = p_1.
  gs_selopt-high  = p_2.

  APPEND gs_selopt TO gt_selopt.
  CLEAR: gs_selopt.

  SELECT * FROM zfk_sap_tbl_01 INTO TABLE gt_tbl WHERE id IN gt_selopt.

  LOOP AT gt_tbl INTO gs_str.

    WRITE: gs_str-id,
           gs_str-name,
           gs_str-surname,
           gs_str-job,
           gs_str-salary,
           gs_str-currency,
           gs_str-email.

  ENDLOOP.

*If we enter the range 2025000002 - 2025000005, p_inc, p_bt, we will get the result as follows:
*    ID     NAME      SURNAME    JOB          SALARY  CURRENCY        EMAIL
*  numc10   char40    char40    char40        int2     char3         char40
*2025000002 DAVID     ALABA     DEVELOPER     4000      EUR   DAVID.ALABA@TRIAL.COM
*2025000004 KAYLE     WALKER    TAXI DRIVER   2350      EUR   KAYLE.WALKER@TRIAL.COM
*2025000005 ALENADER  ARNOLD    ENGINEER      3000      EUR   ALEXANDER.ARNOLD@TRIAL.COM

*If we enter the 2025000002, p_inc, p_eq, we will get the result as follows:
*    ID     NAME      SURNAME    JOB          SALARY  CURRENCY        EMAIL
*  numc10   char40    char40    char40        int2     char3         char40
*2025000002 DAVID     ALABA     DEVELOPER     4000      EUR   DAVID.ALABA@TRIAL.COM


*If we enter the range 2025000002 - 2025000005, p_exc, p_bt, we will get the result as follows:
*    ID     NAME      SURNAME    JOB          SALARY  CURRENCY        EMAIL
*  numc10   char40    char40    char40        int2     char3         char40
*2025000001 HANS      BÖCKLER   DOCTOR        3.500     EUR   HANS.BÖCKLER@TRIAL.COM
*2025000006 COLE      PALMER    LAWYER        2.500     EUR   COLE.PALMER@TRIAL.COM
