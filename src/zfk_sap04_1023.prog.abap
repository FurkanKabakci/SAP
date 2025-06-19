*&---------------------------------------------------------------------*
*& Report ZFK_SAP04_1023
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_sap04_1023.

*The FOR ALL ENTRIES statement is used to retrieve records from a database table
*based on the values available in an internal table.

*Before using FOR ALL ENTRIES, the internal table must not be empty.
*Otherwise, it may return all records from the database table.

*In this report, we will use SAP’s sample tables SCARR, SPFLI, and SFLIGHT, which are related to each other.

DATA: gs_scarr   TYPE scarr,
      gt_scarr   TYPE TABLE OF scarr,
      gs_spfli   TYPE spfli,
      gt_spfli   TYPE TABLE OF spfli,
      gs_sflight TYPE sflight,
      gt_sflight TYPE TABLE OF sflight.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

SELECT-OPTIONS: so_cnnid FOR gs_sflight-connid.

SELECTION-SCREEN END OF BLOCK a1.

*To better understand the process along with its results,
*we will examine the output that appears when we enter
*the value 0017 in the CONNID field under the Select Options section.

START-OF-SELECTION.


  SELECT * FROM sflight INTO TABLE gt_sflight WHERE connid IN so_cnnid.

  LOOP AT gt_sflight INTO gs_sflight.

    WRITE: gs_sflight-carrid,
           gs_sflight-connid,
           gs_sflight-fldate,
           gs_sflight-price,
           gs_sflight-currency.

    CLEAR: gs_sflight.

    SKIP.
  ENDLOOP.

  ULINE.

*Output
*  carrid     connid        fldate        price     currency
*    AA        0017       20.04.2017      422,94      USD
*    AA        0017       22.05.2017      422,94      USD
*    AA        0017       23.06.2017      422,94      USD
*    AA        0017       25.07.2017      422,94      USD
*    AA        0017       26.08.2017      422,94      USD
*    AA        0017       27.09.2017      422,94      USD
*    AA        0017       29.10.2017      422,94      USD
*    AA        0017       30.11.2017      422,94      USD
*    AA        0017       01.01.2018      422,94      USD
*    AA        0017       02.02.2018      422,94      USD
*    AA        0017       06.03.2018      422,94      USD
*    AA        0017       07.04.2018      422,94      USD
*    AA        0017       09.05.2018      422,94      USD

  SELECT * FROM spfli INTO TABLE gt_spfli
                      FOR ALL ENTRIES IN gt_sflight
                      WHERE connid = gt_sflight-connid.

  LOOP AT gt_spfli INTO gs_spfli.

    WRITE: gs_spfli-carrid,
           gs_spfli-connid,
           gs_spfli-cityfrom,
           gs_spfli-cityto.

    CLEAR: gs_spfli.
    SKIP.
  ENDLOOP.

  ULINE.

*Output
*  carrid     connid    cityfrom         cityto
*    AA        0017     NEW YORK       SAN FRANCISCO

  SELECT * FROM scarr INTO TABLE gt_scarr
                      FOR ALL ENTRIES IN gt_spfli
                      WHERE carrid = gt_spfli-carrid.

  LOOP AT gt_scarr INTO gs_scarr.

    WRITE: gs_scarr-carrid,
           gs_scarr-carrname,
           gs_scarr-currcode,
           gs_scarr-url.

    CLEAR: gs_scarr.

    SKIP.
  ENDLOOP.

  ULINE.

*Output
*  carrid        carrname       currcode            url
*    AA     American Airlines     USD       http://www.aa.com
