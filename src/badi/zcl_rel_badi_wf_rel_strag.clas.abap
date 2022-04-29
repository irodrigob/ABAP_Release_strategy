CLASS zcl_rel_badi_wf_rel_strag DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES zif_wfe_badi_workflow_model .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_badi_wf_rel_strag IMPLEMENTATION.
  METHOD zif_wfe_badi_workflow_model~change_step_approvers.

    " Saco los aprobadores según el dueño del paso
    CASE iv_step_owner.
      WHEN zif_rel_data=>cs_wf_engine-step_approvers-ap_approver.
        " En el caso de AP hay que ir a buscar a la tabla de AP donde hay que tener en cuenta si hay aprobadores
        " especificos del departamento/filial.
        DATA(lv_dept_subs) = VALUE zrel_e_depart_subsidiary(  ).
        READ TABLE it_values ASSIGNING FIELD-SYMBOL(<ls_values>) WITH KEY field = zif_rel_data=>cs_wf_engine-field_values-department.
        IF sy-subrc = 0.
          lv_dept_subs = <ls_values>-value.
        ENDIF.

        " Saco quien tiene autorización para realizar la aprobación y lo añado al proceso de aprobación
        DATA(lo_auth) = NEW zcl_rel_authorizations( ).
        lo_auth->get_user_approvers( EXPORTING iv_dept_subs = lv_dept_subs
                                     IMPORTING et_users = DATA(lt_users) ).

        ct_approvers = VALUE #( BASE ct_approvers FOR <wa> IN lt_users ( approver = <wa>-username ) ).

        " En pendiente otros o departamento el aprobador esta en un valor del container
      WHEN zif_rel_data=>cs_wf_engine-step_approvers-ds_approvers.
        READ TABLE it_values ASSIGNING <ls_values> WITH KEY field = zif_rel_data=>cs_wf_engine-field_values-depart_approver.
        IF sy-subrc = 0.
          INSERT VALUE #( approver = <ls_values>-value ) INTO TABLE ct_approvers.
        ENDIF.
    ENDCASE.


  ENDMETHOD.

  METHOD zif_wfe_badi_workflow_model~determine_next_status.
    DATA(lv_cerrar_wf) = abap_false.
    " Solo se determinar el nuevo status en las aprobaciones.
    IF iv_step_result = zif_wfe_data=>cs_wf_process-step_result-approve.
      CASE iv_actual_status.
        WHEN zif_rel_data=>cs_wf_engine-status-pending_ap.
          " Se mira si en el container esta informado el aprobador de departamento. Si no es así, el WF se puede dar por concluido.
          " Si es así, se le indica que vaya al status de pendiente de aprobador.
          READ TABLE it_values TRANSPORTING NO FIELDS WITH KEY field = zif_rel_data=>cs_wf_engine-field_values-depart_approver.
          IF sy-subrc = 0.
            cv_next_status = zif_rel_data=>cs_wf_engine-status-pending_approv_depart.
            cv_workflow_completed = abap_false.
          ELSE.
            lv_cerrar_wf = abap_true.
          ENDIF.
          " Cuando este aprobado por la persona de confirmación volverá de nuevo al aprobador de AP final
        WHEN zif_rel_data=>cs_wf_engine-status-pending_approv_depart.
          cv_next_status = zif_rel_data=>cs_wf_engine-status-pending_ap_final.

      ENDCASE.

      IF lv_cerrar_wf = abap_true.
        cv_next_status = zif_rel_data=>cs_wf_engine-status-completed.
        cv_workflow_completed = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wfe_badi_workflow_model~post_save_workflow.

  ENDMETHOD.

ENDCLASS.
