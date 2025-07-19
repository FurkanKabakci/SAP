class ZFK_CL_SAP04_008 definition
  public
  final
  create public .

public section.

  methods GET_DATA
    importing
      !IT_CARRID type ZFK_TT_CARRID
      !IV_SCARR type CHAR1
      !IV_SPFLI type CHAR1
      !IV_SFLIGHT type CHAR1
    exporting
      !ET_SCARR type ZFK_TT_SCARR
      !ET_SPFLI type ZFK_TT_SPFLI
      !ET_SFLIGHT type ZFK_TT_SFLIGHT .
protected section.

  methods SCARR
    importing
      !IT_CARRID type ZFK_TT_CARRID
    exporting
      !ET_SCARR type ZFK_TT_SCARR .
  methods SPFLI
    importing
      !IT_CARRID type ZFK_TT_CARRID
    exporting
      !ET_SPFLI type ZFK_TT_SPFLI .
  methods SFLIGHT
    importing
      !IT_CARRID type ZFK_TT_CARRID
    exporting
      !ET_SFLIGHT type ZFK_TT_SFLIGHT .
private section.
ENDCLASS.



CLASS ZFK_CL_SAP04_008 IMPLEMENTATION.


  method GET_DATA.

    IF iv_scarr = abap_true.

      scarr(
        EXPORTING
          it_carrid = it_carrid
        IMPORTING
          et_scarr  = et_scarr
      ).
    ENDIF.

    IF iv_spfli = abap_true.

      spfli(
        EXPORTING
          it_carrid =  it_carrid
        IMPORTING
          et_spfli  =     et_spfli
      ).
    ENDIF.

    IF iv_sflight = abap_true.

      sflight(
        EXPORTING
          it_carrid  =  it_carrid
        IMPORTING
          et_sflight =     et_sflight
      ).

    ENDIF.
  endmethod.


  method SCARR.

    SELECT * FROM scarr INTO TABLE et_scarr WHERE carrid IN it_carrid.

  endmethod.


  method SFLIGHT.

    SELECT * FROM sflight INTO TABLE et_sflight WHERE carrid IN it_carrid.

  endmethod.


  method SPFLI.

    SELECT * FROM spfli INTO TABLE et_spfli WHERE carrid IN it_carrid.

  endmethod.
ENDCLASS.
