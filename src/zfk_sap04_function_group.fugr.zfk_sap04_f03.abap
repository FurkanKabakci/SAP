FUNCTION zfk_sap04_f03.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_NUM_1) TYPE  INT4
*"     REFERENCE(IV_NUM_2) TYPE  INT4
*"     REFERENCE(IV_SYMBOL) TYPE  CHAR1
*"  EXPORTING
*"     REFERENCE(EV_RESULT) TYPE  INT4
*"  EXCEPTIONS
*"      DIVISION_BY_ZERO
*"      INVALID_SYMBOL
*"----------------------------------------------------------------------

  IF iv_symbol = '+' .

    ev_result = iv_num_1 + iv_num_2.

  ELSEIF iv_symbol = '-'.

    ev_result = iv_num_1 - iv_num_2.

  ELSEIF iv_symbol = '*'.

    ev_result = iv_num_1 * iv_num_2.

  ELSEIF iv_symbol = '/'.

    IF iv_num_2 = 0.

      RAISE division_by_zero.

    ELSE.

      ev_result = iv_num_1 / iv_num_2.

    ENDIF.

  ELSE.

    RAISE invalid_symbol.

  ENDIF.


ENDFUNCTION.
