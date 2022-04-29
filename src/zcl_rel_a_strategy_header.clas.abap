CLASS zcl_rel_a_strategy_header DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_a_superclass
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_action~execute
        REDEFINITION .
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_log_change_md,
             md_block     TYPE zrel_e_md_block,
             type         TYPE bapi_mtype,
             type_message TYPE bsstring,
             message      TYPE bsstring,
           END OF ts_log_change_md.
    TYPES: tt_log_change_md TYPE STANDARD TABLE OF ts_log_change_md WITH EMPTY KEY.
    DATA mv_langu TYPE sylangu.
    "! <p class="shorttext synchronized">Nuevo workflow</p>
    METHODS new_worflow
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_act
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
        is_parameters TYPE REF TO data
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
        et_data       TYPE INDEX TABLE .
    "! <p class="shorttext synchronized">Envio de mail para la aprobación</p>
    METHODS send_mail_approve
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_act
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
        is_parameters TYPE REF TO data
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
        et_data       TYPE INDEX TABLE .
    "! <p class="shorttext synchronized">Proceso de aprobación</p>
    METHODS approve_process
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_act
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
        is_parameters TYPE REF TO data
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
        et_data       TYPE INDEX TABLE .
    "! <p class="shorttext synchronized">Envio mail de datos maestros modificados</p>
    METHODS send_mail_change_md
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_act
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
        is_parameters TYPE REF TO data
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
        et_data       TYPE INDEX TABLE .
    "! <p class="shorttext synchronized">Crear la foto de los datos de la estrategia que hay en SAP</p>
    METHODS sap_snapshot
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_act
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
        is_parameters TYPE REF TO data
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
        et_data       TYPE INDEX TABLE .
    "! <p class="shorttext synchronized">Rellena simbolos para la plantilla email</p>
    "! @parameter is_header | <p class="shorttext synchronized">Cabecera</p>
    "! @parameter iv_pgroup_desc | <p class="shorttext synchronized">Descripción del grupo de compras</p>
    "! @parameter iv_user_to_date | <p class="shorttext synchronized">Usuario para el formato de fecha</p>
    "! @parameter iv_result_update_sap | <p class="shorttext synchronized">Resultado de la actualización en SAP</p>
    "! @parameter it_log_change_md | <p class="shorttext synchronized">Logs de modificacion de datos maestros</p>
    "! @parameter iv_depart_approver_reason | <p class="shorttext synchronized">Motivo para el aprobador del departamento</p>
    "! @parameter iv_rol_buyer_text | <p class="shorttext synchronized">Rol de comprador</p>
    "! @parameter et_symbols | <p class="shorttext synchronized">Simbolos</p>
    METHODS fill_symbols_template_mail
      IMPORTING
        is_header                 TYPE zrel_bo_sc_strategy_header
        iv_pgroup_desc            TYPE t024-eknam OPTIONAL
        iv_user_to_date           TYPE syuname
        iv_result_update_sap      TYPE string OPTIONAL
        iv_url_cloud              TYPE string OPTIONAL
        it_log_change_md          TYPE tt_log_change_md OPTIONAL
        iv_depart_approver_reason TYPE string OPTIONAL
        iv_rol_buyer_text         TYPE string OPTIONAL
      EXPORTING
        et_symbols                TYPE zca_i_simbolos_mail
        et_symbols_key_table      TYPE zca_i_tabla_simbolos_mail.
    "! <p class="shorttext synchronized">Convierte un valor al formato que espera el simbolo</p>
    "! @parameter io_component | <p class="shorttext synchronized">Componente</p>
    "! @parameter iv_value | <p class="shorttext synchronized">Valor</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario para formato de fecha</p>
    "! @parameter iv_unit | <p class="shorttext synchronized">Unidad para campos de cantidad</p>
    "! @parameter rv_value | <p class="shorttext synchronized">Valor</p>
    METHODS conv_value_2_mail_symbol
      IMPORTING
        io_component    TYPE REF TO cl_abap_elemdescr
        iv_value        TYPE any
        iv_user         TYPE syuname OPTIONAL
        iv_unit         TYPE meins OPTIONAL
      RETURNING
        VALUE(rv_value) TYPE string.
    "! <p class="shorttext synchronized">Obtiene los mails de los aprobadores del paso</p>
    "! @parameter it_steps_approvers | <p class="shorttext synchronized">Aprobadores del paso</p>
    "! @parameter iv_department | <p class="shorttext synchronized">Departamento</p>
    "! @parameter rt_mails | <p class="shorttext synchronized">Mails</p>
    METHODS get_mails_of_approvers
      IMPORTING
        iv_dept_subs       TYPE zrel_e_depart_subsidiary
        it_steps_approvers TYPE zwfe_i_steps_approv_all_fields
      RETURNING
        VALUE(rt_mails)    TYPE bcsy_smtpa.
    "! <p class="shorttext synchronized">Aprueba el paso en el motor de WF</p>
    "! @parameter io_header | <p class="shorttext synchronized">Datos de cabecera</p>
    "! @parameter iv_action | <p class="shorttext synchronized">Acción</p>
    "! @parameter iv_reason | <p class="shorttext synchronized">Motivo de aprobación</p>
    "! @parameter ev_completed | <p class="shorttext synchronized">Completado?</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    "! @parameter ev_next_status | <p class="shorttext synchronized">Siguiente status de aprobación</p>
    METHODS approve_step_wf
      IMPORTING
        iv_action      TYPE zrel_bo_s_approv_params-action
        iv_reason      TYPE string
        io_header      TYPE REF TO zrel_bo_sc_strategy_header
      EXPORTING
        et_return      TYPE zif_wfe_data=>tt_return
        ev_next_status TYPE zwfe_e_status
        ev_completed   TYPE sap_bool.
    "! <p class="shorttext synchronized">Añade el paso de aprobación</p>
    "! @parameter io_header | <p class="shorttext synchronized">Datos de cabecera</p>
    "! @parameter io_modify | <p class="shorttext synchronized">Objeto de actualización del BOPF</p>
    METHODS add_step_approver
      IMPORTING
        io_header TYPE REF TO zrel_bo_sc_strategy_header
        io_modify TYPE REF TO /bobf/if_frw_modify.
    "! <p class="shorttext synchronized">Envio de mail de workflow completado</p>
    METHODS send_mail_approved
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_act
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
        is_parameters TYPE REF TO data
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
        et_data       TYPE INDEX TABLE .
    "! <p class="shorttext synchronized">Construye la tabla de log de cambios datos maestros</p>
    METHODS build_tbl_msg_log
      IMPORTING
        is_ctx            TYPE /bobf/s_frw_ctx_act
        iv_root_key       TYPE zrel_bo_sc_strategy_header-key
        io_read           TYPE REF TO /bobf/if_frw_read
      RETURNING
        VALUE(rt_tbl_log) TYPE zcl_rel_a_strategy_header=>tt_log_change_md.
    "! <p class="shorttext synchronized">Envio de mail para la aprobación por parte del departamento</p>
    METHODS send_mail_approve_depart
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_act
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
        is_parameters TYPE REF TO data
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
        et_data       TYPE INDEX TABLE .
    "! <p class="shorttext synchronized">Lee los aprobadores AP</p>
    METHODS read_ap_approvers
      IMPORTING
                iv_wf_id               TYPE zrel_bo_sc_strategy_header-wf_id
      RETURNING VALUE(rt_ap_approvers) TYPE zwfe_i_steps_approv_all_fields.
    "! <p class="shorttext synchronized">Determina el texto de rol para compradores</p>
    METHODS determine_text_rol_buyers
      IMPORTING
        is_ctx         TYPE /bobf/s_frw_ctx_act
        io_read        TYPE REF TO /bobf/if_frw_read
        iv_key         TYPE zrel_bo_sc_strategy_header-key
      RETURNING
        VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_a_strategy_header IMPLEMENTATION.


  METHOD /bobf/if_frw_action~execute.
    IF eo_message IS NOT BOUND. " Se instancia la clase de mensajes en caso de no estarlo
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

    CASE is_ctx-act_key.
      WHEN zif_rel_bo_strategy_c=>sc_action-root-launch_workflow.
        new_worflow(
          EXPORTING
            is_ctx                  = is_ctx
            it_key                  = it_key
            io_read                 = io_read
            io_modify               = io_modify
            is_parameters           = is_parameters
          IMPORTING
            eo_message              = eo_message
            et_failed_key           =  et_failed_key
            et_data                 = et_data ).
      WHEN zif_rel_bo_strategy_c=>sc_action-root-send_mail_approve.
        send_mail_approve(
           EXPORTING
             is_ctx                  = is_ctx
             it_key                  = it_key
             io_read                 = io_read
             io_modify               = io_modify
             is_parameters           = is_parameters
           IMPORTING
             eo_message              = eo_message
             et_failed_key           =  et_failed_key
             et_data                 = et_data ).
      WHEN zif_rel_bo_strategy_c=>sc_action-root-approve_process.
        approve_process(
           EXPORTING
             is_ctx                  = is_ctx
             it_key                  = it_key
             io_read                 = io_read
             io_modify               = io_modify
             is_parameters           = is_parameters
           IMPORTING
             eo_message              = eo_message
             et_failed_key           =  et_failed_key
             et_data                 = et_data ).
      WHEN zif_rel_bo_strategy_c=>sc_action-root-sap_snapshot.
        sap_snapshot(
           EXPORTING
             is_ctx                  = is_ctx
             it_key                  = it_key
             io_read                 = io_read
             io_modify               = io_modify
             is_parameters           = is_parameters
           IMPORTING
             eo_message              = eo_message
             et_failed_key           =  et_failed_key
             et_data                 = et_data ).
      WHEN zif_rel_bo_strategy_c=>sc_action-root-send_mail_approved.
        send_mail_approved(
           EXPORTING
             is_ctx                  = is_ctx
             it_key                  = it_key
             io_read                 = io_read
             io_modify               = io_modify
             is_parameters           = is_parameters
           IMPORTING
             eo_message              = eo_message
             et_failed_key           =  et_failed_key
             et_data                 = et_data ).
      WHEN zif_rel_bo_strategy_c=>sc_action-root-send_md_change.
        send_mail_change_md(
           EXPORTING
             is_ctx                  = is_ctx
             it_key                  = it_key
             io_read                 = io_read
             io_modify               = io_modify
             is_parameters           = is_parameters
           IMPORTING
             eo_message              = eo_message
             et_failed_key           =  et_failed_key
             et_data                 = et_data ).
      WHEN zif_rel_bo_strategy_c=>sc_action-root-send_mail_depart_approv.
        send_mail_approve_depart(
           EXPORTING
             is_ctx                  = is_ctx
             it_key                  = it_key
             io_read                 = io_read
             io_modify               = io_modify
             is_parameters           = is_parameters
           IMPORTING
             eo_message              = eo_message
             et_failed_key           =  et_failed_key
             et_data                 = et_data ).
    ENDCASE.
  ENDMETHOD.


  METHOD approve_process.

    DATA lt_data TYPE zrel_bo_i_strategy_header.
    DATA lt_return_data TYPE zrel_bo_i_result_approv.

    DATA(lo_params) = NEW zrel_bo_s_approv_params(  ).
    lo_params ?= is_parameters.
    mv_langu = lo_params->langu.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.
      READ TABLE  lt_data REFERENCE INTO DATA(lo_data) INDEX 1.

      " Añado una linea donde se informará del resultado del proceso.
      APPEND INITIAL LINE TO lt_return_data ASSIGNING FIELD-SYMBOL(<ls_return_data>).

      " Primero se realiza la aprobación en el motor workflow
      approve_step_wf( EXPORTING io_header  = lo_data
                                 iv_action = lo_params->action
                                 iv_reason = lo_params->reason
                       IMPORTING et_return = DATA(lt_return_wf)
                                 ev_completed = DATA(lv_completed)
                                 ev_next_status = <ls_return_data>-next_status ).

      " Si hay algun error en la aprobación se devuelve un error indicandolo pero no se modifica nada del proceso.
      READ TABLE lt_return_wf TRANSPORTING NO FIELDS WITH KEY type = zif_wfe_data=>cs_message-type-error.
      IF sy-subrc = 0.
        eo_message->add_message( EXPORTING is_msg = VALUE #( msgid = zif_rel_data=>cs_msg-id
                                                                     msgno = '010'
                                                                     msgty = zif_rel_data=>cs_msg-type_error )
                                                   iv_node = is_ctx-node_key ).
      ELSE.
        " Informo los datos comunes de aprobación solo cuando se completa el workflo o se rechaza.
        "  En estos campos contienen quien ha realizado la última aprobación.
        " En el nodo STEPS_APPROVERS contiene el historial de aprobación que se rellena automáticamente al grabar
        " los datos de cabecera.
        IF lv_completed = abap_true OR lo_params->action = zif_rel_data=>cs_strategy-change_request-approvals-action_approve-reject.
          lo_data->approved_by = sy-uname.
          lo_data->approved_date = sy-datum.
          lo_data->approved_time = sy-uzeit.
          lo_data->approved_reason = lo_params->reason.
        ENDIF.

        " Ahora los status dependen si se ha aprobado o rechazado. Y en caso de aprobación si se ha completado o no.
        IF lo_params->action = zif_rel_data=>cs_strategy-change_request-approvals-action_approve-approve.
          " El mensaje de retorno depende del siguiente paso. Si el siguiente status es pendiente de aprobador por el departamento
          " el mensaje se pondrá que se ha reenviado, aunque realmente, es un aprobación pero si se pone que se apruebe el usuario no lo
          " va a entender.
          IF <ls_return_data>-next_status = zif_rel_data=>cs_wf_engine-status-pending_approv_depart.
            eo_message->add_message( EXPORTING is_msg = VALUE #( msgid = zif_rel_data=>cs_msg-id
                                                                           msgno = '036'
                                                                           msgty = zif_rel_data=>cs_msg-type_success )
                                                         iv_node = is_ctx-node_key ).
          ELSE.
            eo_message->add_message( EXPORTING is_msg = VALUE #( msgid = zif_rel_data=>cs_msg-id
                                                                           msgno = '011'
                                                                           msgty = zif_rel_data=>cs_msg-type_success )
                                                         iv_node = is_ctx-node_key ).
          ENDIF.

          " Si el paso del workflow nos dice que esta completado, se pone el status de aprobado.
          IF lv_completed = abap_true.
            lo_data->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-approved.
          ELSE.
            " Si no esta completado hay que ver hacía donde avanza.

            " Si no esta completado miramos el status siguiente determiando en el WF para saber el status a poner.
            " Si el estado es pendiente de confirmación le indicamos dicho status. Cualquier otro (de momento hay dos de ap) irá
            " a pendiente de aprobación (que es el que ve los de AP).
            IF <ls_return_data>-next_status = zif_rel_data=>cs_wf_engine-status-pending_approv_depart.
              lo_data->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation.
            ELSE.
              lo_data->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending.
            ENDIF.

          ENDIF.

          " Si esta aprobado se cambia el status de datos maestros a pendiente para saber que hay que realizar el proceso.
          IF lo_data->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-approved.
            lo_data->change_md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-pending.
          ENDIF.

          <ls_return_data>-wf_completed = lv_completed.



        ELSE.
          " Si se rechaza se informa el mensaje que la solicitud ha sido rechazada y se cambia el status a rechazado y listos.
          eo_message->add_message( EXPORTING is_msg = VALUE #( msgid = zif_rel_data=>cs_msg-id
                                                               msgno = '012'
                                                               msgty = zif_rel_data=>cs_msg-type_success
                                                               msgv1 = |{ lo_data->request_id ALPHA = OUT }| )
                                             iv_node = is_ctx-node_key ).

          lo_data->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-rejected.
        ENDIF.

        " Añado el registro de aprobación al nodo del BOPF que guarda el log de aprobaciones
        add_step_approver( EXPORTING io_header = lo_data
                                     io_modify = io_modify ).

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).

      ENDIF.

    ENDIF.

    et_data = lt_return_data.

  ENDMETHOD.


  METHOD approve_step_wf.
    CLEAR: et_return.
    ev_completed = abap_false.

    DATA(lo_wf) = NEW zcl_wfe_workflow_engine( ).

    " No se hace ningun commit porque los datos tienen que grabarse a la par que los del BOPF

    " Informo el motivo de aprobación. Aunque este en blanco se informa igualmente.
    lo_wf->update_values(
       EXPORTING
         iv_wf_id  = io_header->wf_id
         it_values = VALUE #( ( field = COND zwfe_e_fieldname( WHEN io_header->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending
                                                               THEN zif_rel_data=>cs_wf_engine-field_values-approval_reason_ap
                                                               ELSE
                                                               COND #( WHEN io_header->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation
                                                                       THEN zif_rel_data=>cs_wf_engine-field_values-approval_reason_depart ) )
                                value = iv_reason ) )
         iv_commit = abap_false ).

    " Se lanza el paso
    lo_wf->continue_workflow_step(
      EXPORTING
        iv_wf_id        = io_header->wf_id
        iv_step_result  = COND #( WHEN iv_action = zif_rel_data=>cs_strategy-change_request-approvals-action_approve-approve
                                  THEN zif_wfe_data=>cs_wf_process-step_result-approve
                                  ELSE zif_wfe_data=>cs_wf_process-step_result-reject )
        iv_commit = abap_false
      IMPORTING
        et_return       = et_return
        ev_wf_completed =  ev_completed
        ev_next_status = ev_next_status ).


  ENDMETHOD.


  METHOD conv_value_2_mail_symbol.
    DATA lv_char10 TYPE c LENGTH 10.
    DATA lv_char20 TYPE c LENGTH 20.

    DATA(ls_field) = io_component->get_ddic_field( p_langu = mv_langu ).

    " Aplico rutina de conversión si fuese necesaria
    IF ls_field-convexit IS NOT INITIAL.
      CASE ls_field-convexit.
        WHEN 'CUNIT'.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              input          = iv_value
              language       = mv_langu
            IMPORTING
              output         = rv_value
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            rv_value = iv_value.
          ENDIF.
        WHEN OTHERS.
          DATA(lv_function) = |CONVERSION_EXIT_{ ls_field-convexit  }_OUTPUT|.
          CALL FUNCTION lv_function
            EXPORTING
              input  = iv_value
            IMPORTING
              output = rv_value.
      ENDCASE.
    ELSE.
      " Si no hay rutina se formato los campos de fecha, hora y cantidad
      CASE ls_field-inttype.
          " No puedo usar el string template para formatearlo y el write to segun el idioma de conexión
          " pasa del formato. Así que a manija.
        WHEN 'D'. "
          DATA(lv_format) = NEW zcl_car_user( )->get_user_date_format( iv_user = iv_user ).
          CASE lv_format.
            WHEN 'DD/MM/YY'.
              rv_value = |{ iv_value+6(2) }/{ iv_value+4(2) }/{ iv_value+2(2) }|.
            WHEN 'DD/MM/YYYY'.
              rv_value = |{ iv_value+6(2) }/{ iv_value+4(2) }/{ iv_value(4) }|.
            WHEN 'MM/DD/YY'.
              rv_value = |{ iv_value+4(2) }/{ iv_value+6(2) }/{ iv_value+2(2) }|.
            WHEN 'MM/DD/YYYY'.
              rv_value = |{ iv_value+4(2) }/{ iv_value+6(2) }/{ iv_value(4) }|.
            WHEN OTHERS.
              WRITE iv_value TO lv_char10.
              rv_value = lv_char10.
          ENDCASE.


        WHEN 'T'. " No puedo usar el string template para formatearlo
          WRITE iv_value TO lv_char10.
          rv_value = lv_char10.
        WHEN 'P'.
          IF iv_unit IS NOT INITIAL.
            WRITE iv_value TO lv_char20 UNIT iv_unit.
            rv_value = |{ lv_char20 ALIGN = LEFT }|.
          ELSE.
            rv_value = |{ iv_value ALIGN = LEFT }|.
          ENDIF.
        WHEN OTHERS.
          rv_value = iv_value.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD fill_symbols_template_mail.

    CLEAR: et_symbols, et_symbols_key_table.

    DATA(lt_components_header) = zcl_ca_utilities=>get_struct_components_recus( is_header ).
    LOOP AT lt_components_header ASSIGNING FIELD-SYMBOL(<ls_components>).
      ASSIGN COMPONENT <ls_components>-name OF STRUCTURE is_header TO FIELD-SYMBOL(<field_header>).
      IF sy-subrc = 0.
        zcl_ca_mail=>set_fill_symbolo_mail( EXPORTING iv_name = <ls_components>-name
                                                      iv_value = conv_value_2_mail_symbol( io_component = CAST cl_abap_elemdescr( <ls_components>-type )
                                                                                           iv_value     = <field_header>
                                                                                           iv_user = iv_user_to_date )
                                            CHANGING ct_symbols = et_symbols ).
      ENDIF.
    ENDLOOP.

    " Texto del grupo de compras
    IF iv_pgroup_desc IS SUPPLIED.
      zcl_ca_mail=>set_fill_symbolo_mail( EXPORTING iv_name = 'PURCHASE_GROUP_DESC'
                                                    iv_value = CONV #( iv_pgroup_desc )
                                          CHANGING ct_symbols = et_symbols ).
    ENDIF.

    " Resultado de la actualización en SAP
    IF iv_result_update_sap IS SUPPLIED.
      zcl_ca_mail=>set_fill_symbolo_mail( EXPORTING iv_name = 'RESULT_UPDATE_SAP'
                                                    iv_value = iv_result_update_sap
                                          CHANGING ct_symbols = et_symbols ).
    ENDIF.

    IF iv_url_cloud IS SUPPLIED.
      zcl_ca_mail=>set_fill_symbolo_mail( EXPORTING iv_name = 'URL_CLOUD_APP'
                                                    iv_value = iv_url_cloud
                                          CHANGING ct_symbols = et_symbols ).
    ENDIF.

    IF it_log_change_md IS NOT INITIAL.

      DATA(lt_components_log) = zcl_ca_utilities=>get_struct_components_recus( it_log_change_md[ 1 ] ).
      DATA(lv_clave_tabla) = VALUE int1( ).

      LOOP AT it_log_change_md ASSIGNING FIELD-SYMBOL(<ls_log_change_md>).
        lv_clave_tabla = lv_clave_tabla + 1.

        APPEND INITIAL LINE TO et_symbols_key_table ASSIGNING FIELD-SYMBOL(<ls_key>).
        <ls_key>-key = lv_clave_tabla.

        LOOP AT lt_components_log ASSIGNING <ls_components>.
          ASSIGN COMPONENT <ls_components>-name OF STRUCTURE <ls_log_change_md> TO FIELD-SYMBOL(<field_log>).
          IF sy-subrc = 0.
            zcl_ca_mail=>set_fill_symbolo_mail( EXPORTING iv_name = <ls_components>-name
                                                          iv_table_key = CONV #( lv_clave_tabla )
                                                          iv_value = conv_value_2_mail_symbol( io_component = CAST cl_abap_elemdescr( <ls_components>-type )
                                                                                               iv_value     = <field_log>
                                                                                               iv_user = iv_user_to_date )
                                                CHANGING ct_symbols = et_symbols ).
          ENDIF.
        ENDLOOP.

      ENDLOOP.
    ENDIF.

    IF iv_depart_approver_reason IS NOT INITIAL.
      zcl_ca_mail=>set_fill_symbolo_mail( EXPORTING iv_name = 'DEPART_APPROVER_REASON'
                                                     iv_value = iv_depart_approver_reason
                                           CHANGING ct_symbols = et_symbols ).
    ENDIF.

    zcl_ca_mail=>set_fill_symbolo_mail( EXPORTING iv_name = 'ROL_BUYER_TEXT'
                                                   iv_value = iv_rol_buyer_text
                                         CHANGING ct_symbols = et_symbols ).

  ENDMETHOD.


  METHOD get_mails_of_approvers.
    DATA lt_r_users TYPE RANGE OF usr21-bname.

    CLEAR: rt_mails.

    lt_r_users = VALUE #( FOR <wa> IN it_steps_approvers ( sign = 'I' option = 'EQ' low = <wa>-approver ) ).

    " Los usuarios administradores no tienen que recibir mail por lo tanto los excluiré de la lista de usuario
    LOOP AT lt_r_users ASSIGNING FIELD-SYMBOL(<ls_r_users>).
      DATA(lv_tabix) = sy-tabix.

      IF NEW zcl_rel_user_authorizations( iv_user = <ls_r_users>-low )->is_admin( iv_dept_subs = iv_dept_subs ) = abap_true.
        DELETE lt_r_users INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    IF lt_r_users IS NOT INITIAL.
      SELECT b~smtp_addr INTO TABLE rt_mails
         FROM usr21 AS a
         LEFT OUTER JOIN adr6 AS b ON
             b~addrnumber = a~addrnumber
             AND b~persnumber = a~persnumber
         WHERE a~bname IN lt_r_users.
    ENDIF.
  ENDMETHOD.


  METHOD new_worflow.
    DATA lt_data TYPE zrel_bo_i_strategy_header.
    DATA lt_return_data TYPE zrel_i_a_exp_new_workflow.

    DATA(lo_params) = NEW zrel_s_a_general_params(  ).
    lo_params ?= is_parameters.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        DATA(lo_wf) = NEW zcl_wfe_workflow_engine( ).

        " Indico que no hago commit porque quiero hacerlo a la vez que se graban los datos del bopf. De esta manera si algo falla se puede hacer un rollback
        " y no se queda nada a medias.
        lo_wf->new_workflow(
         EXPORTING
           iv_workflow = zif_rel_data=>cs_wf_engine-workflow
           it_values   =  VALUE #( ( field = zif_rel_data=>cs_wf_engine-field_values-db_key value = lo_data->key )
                                   ( field = zif_rel_data=>cs_wf_engine-field_values-department value = lo_data->dept_subs )
                                   ( field = zif_rel_data=>cs_wf_engine-field_values-purchase_group value = lo_data->purchase_group )
                                   ( field = zif_rel_data=>cs_wf_engine-field_values-request_id value = lo_data->request_id ) )
           iv_draft    = abap_false
           iv_commit = abap_false
         IMPORTING
           ev_wf_id    = DATA(lv_wf_id)
           et_return   = DATA(lt_return) ).

        " Si hay error en el proceso se devuelve un mensaje indicandolo.
        READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = zif_wfe_data=>cs_message-type-error.
        IF sy-subrc = 0.

          eo_message->add_message( EXPORTING is_msg = VALUE #( msgid = zif_rel_data=>cs_msg-id
                                                               msgno = '006'
                                                               msgty = zif_rel_data=>cs_msg-type_error )
                                             iv_node = is_ctx-node_key ).

        ELSE.

          " Añado el registro de aprobación al nodo del BOPF que guarda el log de aprobaciones
          add_step_approver( EXPORTING io_header = lo_data
                                       io_modify = io_modify ).

          " Sin errores se informa en la cabecera el id de wf generado.
          lo_data->wf_id = lv_wf_id.
          io_modify->update( iv_node = is_ctx-node_key
                             iv_key  = lo_data->key
                             is_data = lo_data ).

          " Inserto en la estructura de salida el id de WF generado
          INSERT VALUE #( wf_id = lv_wf_id ) INTO TABLE lt_return_data.
          et_data = lt_return_data.


          " Mensaje que todo va bien.
          eo_message->add_message( EXPORTING is_msg = VALUE #( msgid = zif_rel_data=>cs_msg-id
                                                                         msgno = '005'
                                                                         msgty = zif_rel_data=>cs_msg-type_success )
                                                       iv_node = is_ctx-node_key ).
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD send_mail_approve.
    DATA lt_data TYPE zrel_bo_i_strategy_header.
    DATA lv_url_cloud TYPE string.

    DATA lv_template TYPE zca_e_plantilla_mail.
    DATA lv_emisor  TYPE ad_smtpadr.

    DATA(lo_params) = NEW zrel_s_a_general_params(  ).
    lo_params ?= is_parameters.
    mv_langu = lo_params->langu.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      " Se recupera el template de la solicitud de aprobación
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'TEMPLATE_APPRV_REQUEST'
                                             IMPORTING ev_value    = lv_template ).

      " Emisor del mail
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'SENDER_MAIL'
                                             IMPORTING ev_value    = lv_emisor ).

      " URL del cloud de la app
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'URL_CLOUD_APP_APROV'
                                             IMPORTING ev_value    = lv_url_cloud ).

      " Lectura de la plantilla de mail
      NEW zcl_ca_plantilla_mail( )->lectura( EXPORTING i_plantilla = lv_template
                                                       i_langu     = lo_params->langu
                                                       iv_appl     = zif_rel_data=>cs_strategy-change_request-templates-appl
                                             IMPORTING e_asunto    = DATA(lv_asunto)
                                                       ev_cuerpo   = DATA(lv_cuerpo) ).

      " El HTML se unscape para poder convertir las variables.
      lv_cuerpo = zcl_ca_http=>unescape_html( lv_cuerpo ).


      DATA(lt_destinatarios) = VALUE bcsy_smtpa( ).

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        " Saco la descripción del grupo de compras
        SELECT SINGLE eknam INTO @DATA(lv_pgroup_desc)
               FROM t024
               WHERE ekgrp = @<ls_data>-purchase_group.


        " Se obtienen los mails de los aprobadores
        DATA(lt_ap_approvers) = read_ap_approvers( EXPORTING iv_wf_id = <ls_data>-wf_id ).
        lt_destinatarios = get_mails_of_approvers( EXPORTING iv_dept_subs = <ls_data>-dept_subs
                                                             it_steps_approvers = lt_ap_approvers ).

        IF lt_destinatarios IS NOT INITIAL.

          " Se rellenan los simbols
          fill_symbols_template_mail( EXPORTING is_header = <ls_data>
                                                iv_pgroup_desc = lv_pgroup_desc
                                                iv_user_to_date = CONV #( lt_ap_approvers[ 1 ]-approver )
                                                iv_url_cloud = lv_url_cloud
                                      IMPORTING et_symbols = DATA(lt_simbolos)
                                                et_symbols_key_table = DATA(lt_simbolos_key_table) ).

          " Envio de mail sin plantilla.
          NEW zcl_ca_mail( )->enviar_sin_plantilla(
                          EXPORTING   i_t_destinatarios    = lt_destinatarios
                                      iv_emisor            = lv_emisor
                                      i_t_simbolos         = lt_simbolos
                                      it_keys              = lt_simbolos_key_table
                                      i_langu              = mv_langu
                                      i_commit             = abap_true
                                      iv_set_long_subjet   = abap_true
                            IMPORTING e_return             = DATA(ls_return)
                            CHANGING cv_asunto = lv_asunto
                                     cv_cuerpo = lv_cuerpo ).

        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD add_step_approver.

    ASSIGN io_header->* TO FIELD-SYMBOL(<ls_header>).

    DATA(lo_step_approver) = NEW zrel_bo_sc_strategy_step_aprov( ).

    ASSIGN lo_step_approver->* TO FIELD-SYMBOL(<ls_step_approver>).
    <ls_step_approver> = CORRESPONDING #( <ls_header> ).
    <ls_step_approver>-ernam = sy-uname.
    <ls_step_approver>-erdat = sy-datum.
    <ls_step_approver>-erzet = sy-uzeit.

    lo_step_approver->key = /bobf/cl_frw_factory=>get_new_key( ).

    " Se añade el registro
    io_modify->create(
      EXPORTING
        iv_node            = zif_rel_bo_strategy_c=>sc_node-step_approvers
        iv_key             = lo_step_approver->key
        is_data            = lo_step_approver
        iv_source_node_key = zif_rel_bo_strategy_c=>sc_node-root
        iv_assoc_key = zif_rel_bo_strategy_c=>sc_association-root-step_approvers
        iv_source_key      = io_header->key  ).

  ENDMETHOD.

  METHOD sap_snapshot.

    DATA lt_data TYPE zrel_bo_i_strategy_header.

    DATA(lo_strategy_md) = NEW zcl_rel_strategy_md_query( iv_langu = mv_langu ).

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      " Sacamos los datos de SAP

      " Compradores
      lo_strategy_md->get_purchase_group_buyer( EXPORTING iv_purchase_group = <ls_data>-purchase_group
                                                IMPORTING et_buyer_purchase_group = DATA(lt_buyers) ).

      LOOP AT lt_buyers ASSIGNING FIELD-SYMBOL(<ls_buyers>) .

        DATA(lo_buyers_sap) = NEW zrel_bo_sc_strategy_buyers_sap(  ).
        ASSIGN lo_buyers_sap->* TO FIELD-SYMBOL(<ls_buyers_bo_sap>).

        " Se pasan los campos de valores.
        <ls_buyers_bo_sap> = CORRESPONDING #( <ls_buyers> ).
        <ls_buyers_bo_sap>-key = /bobf/cl_frw_factory=>get_new_key( ).

        io_modify->create(
             EXPORTING
               iv_node            = zif_rel_bo_strategy_c=>sc_node-buyers_sap
               iv_key             = lo_buyers_sap->key
               is_data            = lo_buyers_sap
               iv_source_node_key = zif_rel_bo_strategy_c=>sc_node-root
               iv_assoc_key = zif_rel_bo_strategy_c=>sc_association-root-buyers_sap
               iv_source_key      = <ls_data>-key  ).


      ENDLOOP.

      " Datos de las estrategias
      lo_strategy_md->get_strate_data_from_pgroup( EXPORTING iv_purchase_group = <ls_data>-purchase_group
                                                   IMPORTING et_strategy_data = DATA(lt_strategies) ).

      " Los datos están en el primer registro
      READ TABLE lt_strategies ASSIGNING FIELD-SYMBOL(<ls_strategies>) INDEX 1.
      IF sy-subrc = 0.

        LOOP AT <ls_strategies>-strategies ASSIGNING FIELD-SYMBOL(<ls_amount>).

          " Primero los importes
          DATA(lo_amount_sap) = NEW zrel_bo_sc_strategy_amount_sap(  ).
          ASSIGN lo_amount_sap->* TO FIELD-SYMBOL(<ls_amount_sap>).
          <ls_amount_sap> = CORRESPONDING #( <ls_amount> ).
          <ls_amount_sap>-key = /bobf/cl_frw_factory=>get_new_key( ).

          io_modify->create( EXPORTING iv_node            = zif_rel_bo_strategy_c=>sc_node-amount_sap
                                       iv_key             = lo_amount_sap->key
                                       is_data            = lo_amount_sap
                                       iv_source_node_key = zif_rel_bo_strategy_c=>sc_node-root
                                       iv_assoc_key = zif_rel_bo_strategy_c=>sc_association-root-amount_sap
                                       iv_source_key      = <ls_data>-key  ).

          " Ahora los aprobadores
          LOOP AT <ls_amount>-approvers ASSIGNING FIELD-SYMBOL(<ls_approvers>).
            DATA(lo_approver_sap) = NEW zrel_bo_sc_strategy_apprv_sap(  ).
            ASSIGN lo_approver_sap->* TO FIELD-SYMBOL(<ls_approver_sap>).
            <ls_approver_sap> = CORRESPONDING #( <ls_approvers> ).
            lo_approver_sap->group = <ls_amount_sap>-group.
            lo_approver_sap->strategy = <ls_amount_sap>-strategy.
            <ls_approver_sap>-key = /bobf/cl_frw_factory=>get_new_key( ).

            io_modify->create( EXPORTING iv_node            = zif_rel_bo_strategy_c=>sc_node-approvers_sap
                                         iv_key             = lo_approver_sap->key
                                         is_data            = lo_approver_sap
                                         iv_source_node_key = zif_rel_bo_strategy_c=>sc_node-root
                                         iv_assoc_key = zif_rel_bo_strategy_c=>sc_association-root-approvers_sap
                                         iv_source_key      = <ls_data>-key  ).
          ENDLOOP.
        ENDLOOP.

      ENDIF.

      " Ahora los datos del grupo de compras
      lo_strategy_md->get_pgroup_info( EXPORTING iv_purchase_group = <ls_data>-purchase_group
                        IMPORTING et_info = DATA(lt_pgroup_info) ).
      READ TABLE lt_pgroup_info ASSIGNING FIELD-SYMBOL(<ls_pgroup_info>) INDEX 1.
      IF sy-subrc = 0.
        DATA(lo_pgroup_sap) = NEW zrel_bo_sc_strategy_pgroup_sap(  ).
        ASSIGN lo_pgroup_sap->* TO FIELD-SYMBOL(<ls_pgroup_sap>).
        " Se pasan los campos de valores.
        <ls_pgroup_sap> = CORRESPONDING #( <ls_pgroup_info> ).
        lo_pgroup_sap->key = /bobf/cl_frw_factory=>get_new_key( ).

        io_modify->create( EXPORTING iv_node            = zif_rel_bo_strategy_c=>sc_node-pgroup_sap
                                           iv_key             = lo_pgroup_sap->key
                                           is_data            = lo_pgroup_sap
                                           iv_source_node_key = zif_rel_bo_strategy_c=>sc_node-root
                                           iv_assoc_key = zif_rel_bo_strategy_c=>sc_association-root-pgroup_sap
                                           iv_source_key      = <ls_data>-key  ).

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD send_mail_approved.
    DATA lt_data TYPE zrel_bo_i_strategy_header.
    DATA lt_data_steps_approvers TYPE zrel_bo_i_strategy_step_aprov.
    DATA lv_template TYPE zca_e_plantilla_mail.
    DATA lv_emisor  TYPE ad_smtpadr.

    DATA(lo_params) = NEW zrel_s_a_general_params(  ).
    lo_params ?= is_parameters.
    mv_langu = lo_params->langu.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      " Emisor del mail
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'SENDER_MAIL'
                                             IMPORTING ev_value    = lv_emisor ).


      " Hago un bucle pero realmente solo hay un solo registro
      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

        " La plantilla dependerá si se ha aprobado o rechazado la solicitud
        zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = COND #( WHEN <ls_data>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-rejected
                                                                               THEN 'TEMPLATE_REJECTED_REQUEST'
                                                                               ELSE 'TEMPLATE_APPROVED_REQUEST' )
                                         IMPORTING ev_value    = lv_template ).
        " Sacamos el mail del solicitante
        SELECT SINGLE b~smtp_addr INTO @DATA(lv_request_mail)
               FROM usr21 AS a LEFT OUTER JOIN adr6 AS b ON
               b~addrnumber = a~addrnumber
               AND b~persnumber = a~persnumber
               WHERE a~bname = @<ls_data>-request_by.

        DATA(lt_destinatarios) = VALUE bcsy_smtpa( ( lv_request_mail ) ).

        " Si esta rechazado tengo que mirar si lo ha rechazado el responsable del departamento. Si es así, hay que añadir
        " los aprobadores de AP para que sepan del rechazo. Si esta aprobada también el AP lo tiene que recibir porque este correo
        " se envia cuando se aprueba y los datos maestros están modificados.
        IF <ls_data>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-rejected
           OR <ls_data>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-approved.

          io_read->retrieve_by_association(
            EXPORTING
              iv_node                 = is_ctx-node_key
              it_key                  = VALUE #( ( key = <ls_data>-key ) )
              iv_association          = zif_rel_bo_strategy_c=>sc_association-root-step_approvers
              iv_fill_data            = abap_true
            IMPORTING
              et_data                 = lt_data_steps_approvers ).


          " Salvo que el AP rechaze la solicitud recibirá la copia porque este mail se recibe cuando los datos maestros
          " han sido actualizados.
          DATA(lv_add) = abap_true.

          " Si se rechaza por parte de AP no llegará al pendiente de confirmación.
          IF <ls_data>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-rejected.
            READ TABLE lt_data_steps_approvers TRANSPORTING NO FIELDS
                                               WITH KEY request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation.
            IF sy-subrc = 0.
              " Ahora miro si el siguiente registro tiene el status de AP, eso quiere decir, que lo ha rechazado AP aún haberlo aprobador el responsable de departamento.
              " En ese caso no se añaden los aprobadores
              DATA(lv_tabix_next) = sy-tabix + 1.

              READ TABLE lt_data_steps_approvers ASSIGNING FIELD-SYMBOL(<ls_data_steps_approvers>) INDEX lv_tabix_next.
              IF sy-subrc = 0.
                IF <ls_data_steps_approvers>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending.
                  lv_add = abap_false.
                ELSE.
                  " Aquí no se si llega porque ahora mismo esta desactivado el reenvio por el pase a producción de la primera fase I del proyecto.
                  " Pero si llega aquí no envio nada de esta manera me evito enviar mail innecesarios
                ENDIF.
              ENDIF.
            ELSE. " AP lo rechaza directamente sin pasar por pendiente de confirmacion.
              lv_add = abap_false.
            ENDIF.
          ELSEIF <ls_data>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-approved.
            DATA(lv_rol_buyer_text) = determine_text_rol_buyers( is_ctx = is_ctx
                                                         io_read = io_read
                                                         iv_key  = <ls_data>-key ).

          ENDIF.

          " Se obtienen los mails de los aprobadores
          IF lv_add = abap_true.
            DATA(lt_destinatarios_ap) = get_mails_of_approvers( EXPORTING it_steps_approvers = read_ap_approvers( EXPORTING iv_wf_id = <ls_data>-wf_id )
                                                                          iv_dept_subs = <ls_data>-dept_subs ).
          ENDIF.

        ENDIF.

        " Saco la descripción del grupo de compras
        SELECT SINGLE eknam INTO @DATA(lv_pgroup_desc)
               FROM t024
               WHERE ekgrp = @<ls_data>-purchase_group.


        " Lectura de la plantilla de mail
        NEW zcl_ca_plantilla_mail( )->lectura( EXPORTING i_plantilla = lv_template
                                                         i_langu     = lo_params->langu
                                                         iv_appl     = zif_rel_data=>cs_strategy-change_request-templates-appl
                                               IMPORTING e_asunto    = DATA(lv_asunto)
                                                         ev_cuerpo   = DATA(lv_cuerpo) ).

        " El HTML se unscape para poder convertir las variables.
        lv_cuerpo = zcl_ca_http=>unescape_html( lv_cuerpo ).

        " Hay un texto de la plantilla que depende si ha ido bien o no la actualización de los datos maestros en sap.
        " Lo dejo preparado por si hay algun texto que ocupado varios numero de mensaje, en el momento del desarrollo solo tiene un numero de mensaje.
        DATA(lv_result_update_sap) = ||.
        IF <ls_data>-change_md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done.
          lv_result_update_sap = zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                                      iv_type = zif_rel_data=>cs_msg-type_success
                                                                      iv_number = '018'
                                                                      iv_langu = lo_params->langu )-message.
        ELSEIF <ls_data>-change_md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-error.
          lv_result_update_sap = zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                                        iv_type = zif_rel_data=>cs_msg-type_success
                                                                        iv_number = '019'
                                                                        iv_langu = lo_params->langu )-message.
        ENDIF.

        " Se rellenan los simbols
        fill_symbols_template_mail( EXPORTING is_header = <ls_data>
                                              iv_pgroup_desc = lv_pgroup_desc
                                              iv_user_to_date = <ls_data>-request_by
                                              iv_result_update_sap = lv_result_update_sap
                                              iv_rol_buyer_text = lv_rol_buyer_text
                                    IMPORTING et_symbols = DATA(lt_simbolos)
                                              et_symbols_key_table = DATA(lt_simbolos_key_table) ).

        " Envio de mail sin plantilla.
        NEW zcl_ca_mail( )->enviar_sin_plantilla(
                        EXPORTING   i_t_destinatarios    = lt_destinatarios
                                    it_destinatarios_cc = lt_destinatarios_ap
                                    iv_emisor            = lv_emisor
                                    i_t_simbolos         = lt_simbolos
                                    it_keys              = lt_simbolos_key_table
                                    i_langu              = mv_langu
                                    i_commit             = abap_true
                                    iv_set_long_subjet   = abap_true
                          IMPORTING e_return             = DATA(ls_return)
                          CHANGING cv_asunto = lv_asunto
                                   cv_cuerpo = lv_cuerpo ).

      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD send_mail_change_md.
    DATA lt_data TYPE zrel_bo_i_strategy_header.
    DATA lv_url_cloud TYPE string.

    DATA lv_template TYPE zca_e_plantilla_mail.
    DATA lv_emisor  TYPE ad_smtpadr.

    DATA(lo_params) = NEW zrel_s_a_general_params(  ).
    lo_params ?= is_parameters.
    mv_langu = lo_params->langu.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      " Se recupera el template de la solicitud de aprobación
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'TEMPLATE_CHANGE_MD'
                                             IMPORTING ev_value    = lv_template ).

      " Emisor del mail
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'SENDER_MAIL'
                                             IMPORTING ev_value    = lv_emisor ).

      " Lectura de la plantilla de mail
      NEW zcl_ca_plantilla_mail( )->lectura( EXPORTING i_plantilla = lv_template
                                                       i_langu     = lo_params->langu
                                                       iv_appl     = zif_rel_data=>cs_strategy-change_request-templates-appl
                                             IMPORTING e_asunto    = DATA(lv_asunto_orig)
                                                       ev_cuerpo   = DATA(lv_cuerpo_orig) ).

      " El HTML se unscape para poder convertir las variables.
      lv_cuerpo_orig = zcl_ca_http=>unescape_html( lv_cuerpo_orig ).

      " Sacamos el mail del solicitante
      SELECT c~username, c~notif_type, b~smtp_addr INTO TABLE @DATA(lt_mails)
             FROM zrel_t023 AS c INNER JOIN usr21 AS a ON
                  a~bname = c~username
              LEFT OUTER JOIN adr6 AS b ON
             b~addrnumber = a~addrnumber
             AND b~persnumber = a~persnumber.



      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

        " Saco la descripción del grupo de compras
        SELECT SINGLE eknam INTO @DATA(lv_pgroup_desc)
               FROM t024
               WHERE ekgrp = @<ls_data>-purchase_group.

        DATA(lt_log) = build_tbl_msg_log( iv_root_key = <ls_data>-key
                                    is_ctx = is_ctx
                                    io_read = io_read ).

        " Añadimos los destintarios que quieren recibir todo.
        DATA(lt_destinatarios) = VALUE bcsy_smtpa( FOR <wa> IN lt_mails WHERE ( notif_type = zif_rel_data=>cs_strategy-master_data-notification-types-all )
                                                                              ( <wa>-smtp_addr ) ).

        " Miro si hay errores en log. Si es así se añaden a los que quieren recibir solo los erroneos.
        READ TABLE lt_log TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
        IF sy-subrc = 0.
          lt_destinatarios = VALUE bcsy_smtpa( BASE lt_destinatarios FOR <wa1> IN lt_mails
                                               WHERE ( notif_type = zif_rel_data=>cs_strategy-master_data-notification-types-error )
                                               ( <wa1>-smtp_addr ) ).
        ENDIF.

        IF lt_destinatarios IS NOT INITIAL.

          " Se rellenan los simbols
          fill_symbols_template_mail( EXPORTING is_header = <ls_data>
                                                iv_pgroup_desc = lv_pgroup_desc
                                                iv_user_to_date = sy-uname
                                                it_log_change_md = lt_log
                                      IMPORTING et_symbols = DATA(lt_simbolos)
                                                et_symbols_key_table = DATA(lt_simbolos_key_table) ).

          " Envio de mail sin plantilla.
          DATA(lv_asunto) = lv_asunto_orig.
          DATA(lv_cuerpo) = lv_cuerpo_orig.
          NEW zcl_ca_mail( )->enviar_sin_plantilla(
                          EXPORTING   i_t_destinatarios    = lt_destinatarios
                                      iv_emisor            = lv_emisor
                                      i_t_simbolos         = lt_simbolos
                                      it_keys              = lt_simbolos_key_table
                                      i_langu              = mv_langu
                                      i_commit             = abap_true
                                      iv_set_long_subjet   = abap_true
                            IMPORTING e_return             = DATA(ls_return)
                            CHANGING cv_asunto = lv_asunto
                                     cv_cuerpo = lv_cuerpo ).

        ENDIF.


      ENDLOOP.

    ENDIF.
  ENDMETHOD.





  METHOD build_tbl_msg_log.
    DATA lt_md_change_status TYPE zrel_bo_i_strategy_md_chng_st.
    DATA lt_md_change_log TYPE zrel_bo_i_strategy_md_log.

    CLEAR: rt_tbl_log.

    " Sacamos el estados de los bloques que se tienen que actualizar.
    io_read->retrieve_by_association(
      EXPORTING
        iv_node                 = is_ctx-node_key
        it_key                  = VALUE #( ( key = iv_root_key ) )
        iv_association          = zif_rel_bo_strategy_c=>sc_association-root-md_change_status
      iv_fill_data            = abap_true
    IMPORTING
      et_data                 = lt_md_change_status ).

    DATA(lv_langu) = COND #( WHEN mv_langu IS INITIAL THEN sy-langu ELSE mv_langu ).

    LOOP AT lt_md_change_status ASSIGNING FIELD-SYMBOL(<ls_md_change_status>).
      " Saco todos los mensajes generados por dicho bloque. Un bloque puede tener logs espaciados en el tiempo por temas de reproceso.

      io_read->retrieve_by_association( EXPORTING iv_node                 = zif_rel_bo_strategy_c=>sc_node-md_change_status
                                                  it_key                  = VALUE #( ( key = <ls_md_change_status>-key ) )
                                                  iv_association          = zif_rel_bo_strategy_c=>sc_association-md_change_status-md_change_log
                                                  iv_fill_data            = abap_true
                                        IMPORTING et_data                 = lt_md_change_log ).

      " En el bloque me guardo la ultima clave del mensaje generado. El motivo es para ahora saber la usuario, fecha y hora y devolver los ultimos mensajes.
      " hay que tener en cuenta que los campos de usuario, fecha y hora se informar en el propio BOPF por lo tanto si hay más de un mensaje se guarda con los mismos datos.
      " Es verdad, que generalmente habrá un solo mensaje pero los casos más complejos (importe o aprobadores) puede que haya más mensajes y quiero que se vean todos
      READ TABLE lt_md_change_log ASSIGNING FIELD-SYMBOL(<ls_md_change_log_last>) WITH KEY key = <ls_md_change_status>-key_last_log.
      IF sy-subrc = 0.
        LOOP AT lt_md_change_log ASSIGNING FIELD-SYMBOL(<ls_md_change_log>) WHERE ernam = <ls_md_change_log_last>-ernam
                                                                                  AND erdat = <ls_md_change_log_last>-erdat
                                                                                  AND erzet = <ls_md_change_log_last>-erzet.
          INSERT VALUE #( md_block = <ls_md_change_log>-md_block
                          type = <ls_md_change_log>-type
                          type_message = zcl_ca_utilities=>fill_return( iv_type = zif_rel_data=>cs_msg-type_success
                                                                        iv_id = zif_rel_data=>cs_msg-id
                                                                        iv_langu = lv_langu
                                                                        iv_number = COND #( WHEN <ls_md_change_log>-type = zif_rel_data=>cs_msg-type_success
                                                                                            THEN '024' ELSE '023' )  )-message
                          message = zcl_ca_utilities=>fill_return( iv_type = <ls_md_change_log>-type
                                                                   iv_id = <ls_md_change_log>-id
                                                                   iv_langu = lv_langu
                                                                   iv_number = <ls_md_change_log>-number
                                                                   iv_message_v1 = <ls_md_change_log>-message_v1
                                                                   iv_message_v2 = <ls_md_change_log>-message_v2
                                                                   iv_message_v3 = <ls_md_change_log>-message_v3
                                                                   iv_message_v4 = <ls_md_change_log>-message_v4  )-message ) INTO TABLE rt_tbl_log.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD send_mail_approve_depart.
    DATA lt_data TYPE zrel_bo_i_strategy_header.
    DATA lv_url_cloud TYPE string.

    DATA lv_template TYPE zca_e_plantilla_mail.
    DATA lv_emisor  TYPE ad_smtpadr.

    DATA(lo_params) = NEW zrel_bo_s_approv_depart(  ).
    lo_params ?= is_parameters.
    mv_langu = lo_params->langu.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      " Se recupera el template de la solicitud de aprobación
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'TEMPLATE_APPRV_REQUEST_DEPART'
                                             IMPORTING ev_value    = lv_template ).

      " Emisor del mail
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'SENDER_MAIL'
                                             IMPORTING ev_value    = lv_emisor ).

      " URL del cloud de la app
      zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'URL_CLOUD_APP_APROV'
                                             IMPORTING ev_value    = lv_url_cloud ).

      " Lectura de la plantilla de mail
      NEW zcl_ca_plantilla_mail( )->lectura( EXPORTING i_plantilla = lv_template
                                                       i_langu     = lo_params->langu
                                                       iv_appl     = zif_rel_data=>cs_strategy-change_request-templates-appl
                                             IMPORTING e_asunto    = DATA(lv_asunto)
                                                       ev_cuerpo   = DATA(lv_cuerpo) ).

      " El HTML se unscape para poder convertir las variables.
      lv_cuerpo = zcl_ca_http=>unescape_html( lv_cuerpo ).

      DATA(lo_query) = NEW zcl_wfe_model_data_query( ).
      DATA(lt_destinatarios) = VALUE bcsy_smtpa( ).

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        " Saco la descripción del grupo de compras
        SELECT SINGLE eknam INTO @DATA(lv_pgroup_desc)
               FROM t024
               WHERE ekgrp = @<ls_data>-purchase_group.


        " Se obtienen los mails de los aprobadores
        lt_destinatarios = get_mails_of_approvers( EXPORTING it_steps_approvers = VALUE #( ( approver = lo_params->depart_approver ) )
                                                             iv_dept_subs = <ls_data>-dept_subs ).

        IF lt_destinatarios IS NOT INITIAL.

          " Se rellenan los simbols
          fill_symbols_template_mail( EXPORTING is_header = <ls_data>
                                                iv_pgroup_desc = lv_pgroup_desc
                                                iv_user_to_date = lo_params->depart_approver
                                                iv_url_cloud = lv_url_cloud
                                                iv_depart_approver_reason = lo_params->depart_approver_reason
                                      IMPORTING et_symbols = DATA(lt_simbolos)
                                                et_symbols_key_table = DATA(lt_simbolos_key_table) ).

          " Envio de mail sin plantilla.
          NEW zcl_ca_mail( )->enviar_sin_plantilla(
                          EXPORTING   i_t_destinatarios    = lt_destinatarios
                                      iv_emisor            = lv_emisor
                                      i_t_simbolos         = lt_simbolos
                                      it_keys              = lt_simbolos_key_table
                                      i_langu              = mv_langu
                                      i_commit             = abap_true
                                      iv_set_long_subjet   = abap_true
                            IMPORTING e_return             = DATA(ls_return)
                            CHANGING cv_asunto = lv_asunto
                                     cv_cuerpo = lv_cuerpo ).

        ENDIF.


      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD read_ap_approvers.

    CLEAR: rt_ap_approvers.

    DATA(lo_query) = NEW zcl_wfe_model_data_query( ).
    lo_query->get_steps_approvers_data( EXPORTING it_params_sl       = VALUE #( ( selname = zif_wfe_data=>cs_model_data-steps_approvers-fields-wf_id
                                                                                  kind = 'S'
                                                                                  sign = 'I'
                                                                                  option = 'EQ'
                                                                                  low = iv_wf_id ) )
                                        IMPORTING et_steps_approvers = rt_ap_approvers ).

  ENDMETHOD.


  METHOD determine_text_rol_buyers.
    DATA lt_data_buyers TYPE zrel_bo_i_strategy_buyers.

    CLEAR: rv_text.

    io_read->retrieve_by_association(
                  EXPORTING
                    iv_node                 = is_ctx-node_key
                    it_key                  = VALUE #( ( key = iv_key ) )
                    iv_association          = zif_rel_bo_strategy_c=>sc_association-root-buyers
                    iv_fill_data            = abap_true
                  IMPORTING
                    et_data                 = lt_data_buyers ).

    " Si se añaden compradores o modifican devuelvo el texto advirtiendo que el rol se asignará
    " en un plazo de 24 horas
    LOOP AT lt_data_buyers TRANSPORTING NO FIELDS WHERE cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert
                                                        OR cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-update.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      rv_text = |{ zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                  iv_type = zif_rel_data=>cs_msg-type_success
                                                  iv_number = '037' )-message } { zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                  iv_type = zif_rel_data=>cs_msg-type_success
                                                  iv_number = '038' )-message }|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
