*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1026
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1026.

*In SAP, functions are modular components used to perform specific operations within the system,
*often handling repetitive tasks in a reusable way. Function modules are created through SE37 – Function Builder
*and can be integrated into ABAP reports written in SE38 by using the CALL FUNCTION pattern option.
*
*Within a function module, input data is defined as Importing parameters, and output data after processing is
*defined as Exporting parameters. These parameters are typically named with prefixes like IV (Importing Variable)
*and EV (Exporting Variable). However, when calling the function within SE38, these parameter names may appear in
*reverse — for example, EV for incoming values and IV for outgoing ones — depending on the context.
*
*Additionally, exceptions can be defined within the function module to handle errors or specific conditions,
*making the report more robust and controlled.

PARAMETERS: p_num_1  TYPE i,
            p_num_2  TYPE i,
            p_symbol TYPE c LENGTH 1.

DATA: gv_result TYPE i.

CALL FUNCTION 'ZFK_SAP04_F03'
  EXPORTING
    iv_num_1               = p_num_1
    iv_num_2               = p_num_2
    iv_symbol              = p_symbol
 IMPORTING
   EV_RESULT              = gv_result
 EXCEPTIONS
   DIVISION_BY_ZERO       = 1
   INVALID_SYMBOL         = 2
   OTHERS                 = 3
          .
IF sy-subrc = 1.
  MESSAGE | Division by zero is undefined in mathematics. | TYPE 'S' DISPLAY LIKE 'E'.
ELSEIF sy-subrc = 2.
  MESSAGE | Invalid Symbol. | TYPE 'S' DISPLAY LIKE 'E'.
ELSEIF sy-subrc = 0.
  MESSAGE | Result: { gv_result } | TYPE 'I'.
ENDIF.
