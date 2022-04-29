CLASS zcl_rel_filter_depart_pgroup DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        !iv_langu TYPE sylangu DEFAULT sy-langu .

    "! <p class="shorttext synchronized">Obtiene los datos de los filtros pasado por parámetro</p>
    "! @parameter et_filters_data | <p class="shorttext synchronized">Datos de los filtros</p>
    METHODS get_data
      RETURNING VALUE(rt_filter_data) TYPE zcl_ca_manag_filters=>tt_data_response.
  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_filter_depart_pgroup IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.
  ENDMETHOD.

  METHOD get_data.
    DATA(lo_md) = NEW zcl_rel_strategy_md_query( iv_langu = mv_langu ).

    CLEAR rt_filter_data.

    lo_md->get_pgroup_from_dept_subs( IMPORTING et_purchase_group = DATA(lt_purchase_group) ).

    IF lt_purchase_group IS NOT INITIAL.

      INSERT VALUE #( filter_id = zcl_rel_filters=>mv_filter_depart_subs )
             INTO TABLE rt_filter_data
             ASSIGNING FIELD-SYMBOL(<ls_depart>).

      INSERT VALUE #( filter_id = zcl_rel_filters=>mv_filter_purchase_group )
             INTO TABLE rt_filter_data
             ASSIGNING FIELD-SYMBOL(<ls_pgroup>).

      LOOP AT lt_purchase_group ASSIGNING FIELD-SYMBOL(<ls_purchase_group_dummy>)
                                 GROUP BY ( dept_subs = <ls_purchase_group_dummy>-dept_subs )
                                 ASSIGNING FIELD-SYMBOL(<group>).
        INSERT VALUE #( code = <group>-dept_subs )
               INTO TABLE <ls_depart>-items
               ASSIGNING FIELD-SYMBOL(<ls_depart_items>).

        LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<ls_purchase_group>).
          <ls_depart_items>-desc = <ls_purchase_group>-dept_subs_desc.

          INSERT VALUE #( code = <ls_purchase_group>-purchase_group
                          desc = <ls_purchase_group>-purchase_group_desc
                          value_parent1 = <ls_purchase_group>-dept_subs )
                 INTO TABLE <ls_pgroup>-items.

        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
