*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1025
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1025.

*Within a program, we can organize specific blocks of code by writing them into includes that we create.
*This makes the report shorter, more structured, and significantly easier to analyze compared to its non-encapsulated version.
*Furthermore, encapsulation is used to protect data from external influences and to ensure that it can only be accessed through defined methods.

*We have reorganized the simple calculator code we created in the previous report using this approach,
*making it more structured and easier to read.

*We can create INCLUDE definitions by double-clicking on the include name after writing it in the report.
*Alternatively, they can also be created via SE38. However, an important point to note is that
*the program type must be set to Include Program, not Executable.

INCLUDE zfk_sap04_1025_top. "For data declarations and parameters, we usually add the suffix '_top' to the end of the report name.

INCLUDE zfk_sap04_1025_f01. "For form routines and other called procedures, we typically name the include file by adding f01 to the end of the report name.

START-OF-SELECTION.

  IF p_symbol = '+'.

    PERFORM sum.

  ELSEIF p_symbol = '-'.

    PERFORM sub.

  ELSEIF p_symbol = '*'.

    PERFORM multiply.

  ELSEIF p_symbol = '/'.

    PERFORM divide.

  ELSE.

    MESSAGE 'Invalid Operation' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.
