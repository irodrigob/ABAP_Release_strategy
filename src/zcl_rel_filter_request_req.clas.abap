CLASS zcl_rel_filter_request_req DEFINITION
  PUBLIC
  INHERITING FROM zcl_ca_filters
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_ca_filters~get_data REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_filter_request_req IMPLEMENTATION.
  METHOD zif_ca_filters~get_data.

    CLEAR: et_data.

    SELECT DISTINCT a~request_by AS code
           c~name_text AS desc
           INTO CORRESPONDING FIELDS OF TABLE et_data
           FROM zrel_t010 AS a INNER JOIN usr21 AS b ON
                b~bname = a~request_by
                INNER JOIN adrp AS c ON
                c~persnumber = b~persnumber
                ORDER BY c~name_text.


  ENDMETHOD.

ENDCLASS.
