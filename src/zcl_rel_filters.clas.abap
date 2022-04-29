CLASS zcl_rel_filters DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA mv_filter_depart_subs TYPE zca_e_filter_id READ-ONLY.
    CLASS-DATA mv_filter_purchase_group TYPE zca_e_filter_id READ-ONLY.
    CLASS-DATA mv_filter_status TYPE zca_e_filter_id READ-ONLY.
    CLASS-DATA mv_filter_req_date TYPE zca_e_filter_id READ-ONLY.
    CLASS-DATA mv_filter_req_id TYPE zca_e_filter_id READ-ONLY.
    CLASS-DATA mv_filter_requester TYPE zca_e_filter_id READ-ONLY.
    CLASS-DATA mv_filter_approver TYPE zca_e_filter_id READ-ONLY.

    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        !iv_langu TYPE sylangu DEFAULT sy-langu .
    "! <p class="shorttext synchronized">Obtiene los filtros de configuración</p>
    "! @parameter et_filters | <p class="shorttext synchronized">Filtros</p>
    METHODS get_filters_config
      EXPORTING
        !et_filters TYPE zcl_ca_manag_filters=>tt_filter_conf_ext .
    "! <p class="shorttext synchronized">Obtiene los datos de los filtros pasado por parámetro</p>
    "! @parameter it_filters_request | <p class="shorttext synchronized">Filtros solicitados</p>
    "! @parameter et_filters_data | <p class="shorttext synchronized">Datos de los filtros</p>
    METHODS get_filters_data
      IMPORTING
        !it_filters_request TYPE zcl_ca_manag_filters=>tt_data_request
      EXPORTING
        !et_filters_data    TYPE zcl_ca_manag_filters=>tt_data_response .
  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
    DATA mo_manag_filter TYPE REF TO zcl_ca_manag_filters.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_filters IMPLEMENTATION.


  METHOD class_constructor.

    " Informo los filtros de la tabla de constante
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'FILTER_DEPT_SUBS'
                                           IMPORTING ev_value = mv_filter_depart_subs ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'FILTER_PURCHASE_GROUP'
                                           IMPORTING ev_value = mv_filter_purchase_group ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'FILTER_STATUS'
                                           IMPORTING ev_value = mv_filter_status ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'FILTER_REQ_DATE'
                                           IMPORTING ev_value = mv_filter_req_date ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'FILTER_REQ_ID'
                                           IMPORTING ev_value = mv_filter_req_id ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'FILTER_REQUESTER'
                                           IMPORTING ev_value = mv_filter_requester ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'FILTER_APPROVER'
                                           IMPORTING ev_value = mv_filter_approver ).
  ENDMETHOD.


  METHOD constructor.
    mv_langu = iv_langu.

    mo_manag_filter = NEW zcl_ca_manag_filters( iv_appl = zif_rel_data=>cv_app
                                                iv_langu = mv_langu ).
  ENDMETHOD.


  METHOD get_filters_config.

    CLEAR: et_filters.

    SELECT filter_id INTO TABLE @DATA(lt_filter_id)
           FROM zca_t_filters
           WHERE appl = @zif_rel_data=>cv_app.

    mo_manag_filter->get_config(
      EXPORTING
        it_filters_id = VALUE #( FOR <wa> IN lt_filter_id ( <wa>-filter_id )  )
      IMPORTING
        et_config     = et_filters ).

    SORT et_filters BY order_filter.


  ENDMETHOD.


  METHOD get_filters_data.

    DATA(lt_filters_request) = it_filters_request.

    " Los valores de los filtros se obtendrán de manera estándar salvo el de grupo de compras
    " y departamentos que irán de la mano.
    LOOP AT lt_filters_request TRANSPORTING NO FIELDS
                               WHERE filter_id = mv_filter_depart_subs
                                     OR filter_id = mv_filter_purchase_group.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      " Saco los filtros de la tabla para que no se procesen por la vía estándar
      DELETE lt_filters_request WHERE filter_id = mv_filter_depart_subs
                                       OR filter_id = mv_filter_purchase_group.

      DATA(lt_values) = NEW zcl_rel_filter_depart_pgroup( iv_langu = mv_langu )->get_data(  ).
      INSERT LINES OF lt_values INTO TABLE et_filters_data.
    ENDIF.

    " Se procesan el restro de filtros.
    mo_manag_filter->execute(
   EXPORTING
     it_data_request  =   lt_filters_request
   IMPORTING
      et_data_response =  DATA(lt_filters_data) ).

    INSERT LINES OF lt_filters_data INTO TABLE et_filters_data.

  ENDMETHOD.
ENDCLASS.
