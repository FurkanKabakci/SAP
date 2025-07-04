*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1036
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1036.

*Classes in ABAP can be defined globally using transaction SE24, or locally within a report.

*Local classes are defined at the beginning of the report using the CLASS ... DEFINITION and
*CLASS ... IMPLEMENTATION sections. After these sections are completed, the rest of the code
*can interact with the class in the same way as with a global class — by creating an instance
*and calling its methods.

CLASS lcl_simple_calculator DEFINITION.

*The CLASS ... DEFINITION section is where we specify the method signatures,
*including input and output parameters.

  PUBLIC SECTION.  "Here, we define the access level of the methods — whether they are public, protected, or private.

*When we use METHODS:, we define instance methods that require an object to be called.
*When we use CLASS-METHODS:, we define static methods, which can be called without creating an instance of the class.

    METHODS:  sum        IMPORTING iv_num_1  TYPE i
                                   iv_num_2  TYPE i
                         EXPORTING ev_result TYPE i,

              sub        IMPORTING iv_num_1  TYPE i
                                   iv_num_2  TYPE i
                         EXPORTING ev_result TYPE i,

              multiply   IMPORTING iv_num_1  TYPE i
                                   iv_num_2  TYPE i
                         EXPORTING ev_result TYPE i,

              divide     IMPORTING iv_num_1  TYPE i
                                   iv_num_2  TYPE i
                         EXPORTING ev_result TYPE i
                         EXCEPTIONS division_by_zero.
ENDCLASS.

CLASS lcl_simple_calculator IMPLEMENTATION.

*The CLASS ... IMPLEMENTATION section is where we define the actual
*logic of the methods that were declared in the definition section.

  METHOD sum.
    ev_result = iv_num_1 + iv_num_2.
    MESSAGE | Result: { ev_result } | TYPE 'I'.
  ENDMETHOD.

  METHOD sub.
    ev_result = iv_num_1 - iv_num_2.
    MESSAGE | Result: { ev_result } | TYPE 'I'.
  ENDMETHOD.

  METHOD multiply.
    ev_result = iv_num_1 * iv_num_2.
    MESSAGE | Result: { ev_result } | TYPE 'I'.
  ENDMETHOD.

  METHOD divide.

    IF iv_num_2 = 0.
      RAISE division_by_zero.
    ELSE.
      ev_result = iv_num_1 / iv_num_2.
      MESSAGE | Result: { ev_result } | TYPE 'I'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001 NO INTERVALS.
PARAMETERS: p_num_1  TYPE i,
            p_num_2  TYPE i,
            p_symbol TYPE c LENGTH 1.
SELECTION-SCREEN END OF BLOCK a1.

DATA: go_calculator TYPE REF TO lcl_simple_calculator.

START-OF-SELECTION.

  CREATE OBJECT go_calculator.

  CASE p_symbol.
    WHEN '+'.
      go_calculator->sum(
        EXPORTING
          iv_num_1 = p_num_1
          iv_num_2 = p_num_2 ).

    WHEN '-'.
      go_calculator->sub(
        EXPORTING
          iv_num_1 = p_num_1
          iv_num_2 = p_num_2 ).

    WHEN '*'.
      go_calculator->multiply(
        EXPORTING
          iv_num_1 = p_num_1
          iv_num_2 = p_num_2 ).

    WHEN '/'.
      go_calculator->divide(
        EXPORTING
          iv_num_1 = p_num_1
          iv_num_2 = p_num_2
        EXCEPTIONS
          division_by_zero = 1
          OTHERS           = 2 ).

        IF sy-subrc = 1.
          MESSAGE | Division by zero is undefined in mathematics. | TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

    WHEN OTHERS.

      MESSAGE | Invalid Symbol. | TYPE 'S' DISPLAY LIKE 'E'.

  ENDCASE.
