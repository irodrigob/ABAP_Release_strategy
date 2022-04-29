CLASS zcl_rel_filter_request_status DEFINITION
  PUBLIC
  INHERITING FROM zcl_ca_filters
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_ca_filters~get_data REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_filter_request_status IMPLEMENTATION.
  METHOD zif_ca_filters~get_data.

    CLEAR: et_data.

    SELECT domvalue_l AS code, ddtext AS desc INTO CORRESPONDING FIELDS OF TABLE @et_data
              FROM dd07t
              WHERE domname = @zif_rel_data=>cs_strategy-change_request-approvals-request_status-domain
                    AND ddlanguage = @iv_langu.

    " Se marcan por defecto los estados pendientes.
    LOOP AT et_data ASSIGNING FIELD-SYMBOL(<ls_data>)
                    WHERE code = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending
                          OR code = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation.
      <ls_data>-default_selected = abap_true.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
