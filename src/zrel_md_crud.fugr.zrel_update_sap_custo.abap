FUNCTION ZREL_UPDATE_SAP_CUSTO.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(IT_T16FS) TYPE  ZREL_I_T16FS OPTIONAL
*"     VALUE(IT_T16FT) TYPE  ZREL_I_T16FT OPTIONAL
*"     VALUE(IV_COMMIT) TYPE  SAP_BOOL DEFAULT ABAP_TRUE
*"     VALUE(IV_LANGU) TYPE  SYLANGU DEFAULT SY-LANGU
*"     VALUE(IT_T16FK) TYPE  ZREL_I_T16FK OPTIONAL
*"     VALUE(IT_T16FV) TYPE  ZREL_I_T16FV OPTIONAL
*"     VALUE(IT_T16FW) TYPE  ZREL_I_T16FW OPTIONAL
*"     VALUE(IT_T16FD) TYPE  ZREL_I_T16FD OPTIONAL
*"     VALUE(IT_T16FC) TYPE  ZREL_I_T16FC OPTIONAL
*"     VALUE(IT_T16FW_DELETE) TYPE  ZREL_I_T16FW OPTIONAL
*"     VALUE(IT_T16FK_DELETE) TYPE  ZREL_I_T16FK OPTIONAL
*"     VALUE(IT_T16FV_DELETE) TYPE  ZREL_I_T16FV OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  DATA lv_tabname TYPE tabname.

  CLEAR: et_return.

  " Primero lanzamos los procesos de borrado antes de hacer las actualizaciones
  " Borrado de la tabla de requisitos de liberación
  IF it_t16fw_delete IS NOT INITIAL.
    DELETE t16fw FROM TABLE it_t16fw_delete.
    " El borrado no controlo si va bien porque con la historia de reprocesos puede ser que en algun sistema
    " se haya borrado previamente y fastidie todo el proceso. Además, solo se añade mensaje si no se ha actualizado
    " la propia tabla.
    IF sy-subrc = 0 AND it_t16fw_delete IS INITIAL.
      INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                  iv_id         = zif_rel_data=>cs_msg-id
                                  iv_number     = '032'
                                  iv_langu      = iv_langu
                                  iv_message_v1 = sy-sysid
                                  iv_message_v2 = 'T16FW' ) INTO TABLE et_return.

    ENDIF.
  ENDIF.

  " Borrado de la tabla de los estados de liberación
  IF it_t16fk_delete IS NOT INITIAL.
    DELETE t16fk FROM TABLE it_t16fk_delete.
    " El borrado no controlo si va bien porque con la historia de reprocesos puede ser que en algun sistema
    " se haya borrado previamente y fastidie todo el proceso. Además, solo se añade mensaje si no se ha actualizado
    " la propia tabla.
    IF sy-subrc = 0 AND it_t16fk_delete IS INITIAL.
      INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                  iv_id         = zif_rel_data=>cs_msg-id
                                  iv_number     = '032'
                                  iv_langu      = iv_langu
                                  iv_message_v1 = sy-sysid
                                  iv_message_v2 = 'T16FK' ) INTO TABLE et_return.

    ENDIF.
  ENDIF.

  " Borrado de la tabla de los requisitos de liberación
  IF it_t16fv_delete IS NOT INITIAL.
    DELETE t16fv FROM TABLE it_t16fv_delete.
    " El borrado no controlo si va bien porque con la historia de reprocesos puede ser que en algun sistema
    " se haya borrado previamente y fastidie todo el proceso. Además, solo se añade mensaje si no se ha actualizado
    " la propia tabla.
    IF sy-subrc = 0 AND it_t16fv_delete IS INITIAL.
      INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                  iv_id         = zif_rel_data=>cs_msg-id
                                  iv_number     = '032'
                                  iv_langu      = iv_langu
                                  iv_message_v1 = sy-sysid
                                  iv_message_v2 = 'T16FV' ) INTO TABLE et_return.

    ENDIF.
  ENDIF.

  " Tabla de grupos y estrategias de liberación
  IF it_t16fs IS NOT INITIAL.
    MODIFY t16fs FROM TABLE it_t16fs.
    IF sy-subrc = 0.
      INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                   iv_id         = zif_rel_data=>cs_msg-id
                                   iv_number     = '032'
                                   iv_langu      = iv_langu
                                   iv_message_v1 = sy-sysid
                                   iv_message_v2 = 'T16FS' ) INTO TABLE et_return.
    ELSE.

      INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                          iv_id         = zif_rel_data=>cs_msg-id
                                          iv_number     = '031'
                                          iv_langu      = iv_langu
                                          iv_message_v1 = sy-sysid
                                          iv_message_v2 = 'T16FS' ) INTO TABLE et_return.

    ENDIF.
  ENDIF.

  " Si no hay errores se continua con la siguiente tabla
  IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_success ] ).
    " Tabla de textos de grupos y estrategias de liberación
    IF it_t16ft IS NOT INITIAL.
      MODIFY t16ft FROM TABLE it_t16ft.
      IF sy-subrc = 0.
        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                    iv_id         = zif_rel_data=>cs_msg-id
                                    iv_number     = '032'
                                    iv_langu      = iv_langu
                                    iv_message_v1 = sy-sysid
                                    iv_message_v2 = 'T16FT' ) INTO TABLE et_return.
      ELSE.

        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '031'
                                            iv_langu      = iv_langu
                                            iv_message_v1 = sy-sysid
                                            iv_message_v2 = 'T16FT' ) INTO TABLE et_return.


      ENDIF.
    ENDIF.
  ENDIF.

  " Si no hay errores se continua con la siguiente tabla
  IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_success ] ).
    " Tabla de estados de liberación
    IF it_t16fk IS NOT INITIAL.
      MODIFY t16fk FROM TABLE it_t16fk.
      IF sy-subrc = 0.
        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                    iv_id         = zif_rel_data=>cs_msg-id
                                    iv_number     = '032'
                                    iv_langu      = iv_langu
                                    iv_message_v1 = sy-sysid
                                    iv_message_v2 = 'T16FK' ) INTO TABLE et_return.
      ELSE.

        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '031'
                                            iv_langu      = iv_langu
                                            iv_message_v1 = sy-sysid
                                            iv_message_v2 = 'T16FK' ) INTO TABLE et_return.


      ENDIF.
    ENDIF.
  ENDIF.

  " Si no hay errores se continua con la siguiente tabla
  IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_success ] ).
    " Tabla de requisitos de liberación
    IF it_t16fv IS NOT INITIAL.
      MODIFY t16fv FROM TABLE it_t16fv.
      IF sy-subrc = 0.
        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                    iv_id         = zif_rel_data=>cs_msg-id
                                    iv_number     = '032'
                                    iv_langu      = iv_langu
                                    iv_message_v1 = sy-sysid
                                    iv_message_v2 = 'T16FV' ) INTO TABLE et_return.
      ELSE.

        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '031'
                                            iv_langu      = iv_langu
                                            iv_message_v1 = sy-sysid
                                            iv_message_v2 = 'T16FV' ) INTO TABLE et_return.


      ENDIF.
    ENDIF.
  ENDIF.

  " Si no hay errores se continua con la siguiente tabla
  IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_success ] ).
    " Tabla de requisitos de liberación
    IF it_t16fw IS NOT INITIAL.
      MODIFY t16fw FROM TABLE it_t16fw.
      IF sy-subrc = 0.
        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                    iv_id         = zif_rel_data=>cs_msg-id
                                    iv_number     = '032'
                                    iv_langu      = iv_langu
                                    iv_message_v1 = sy-sysid
                                    iv_message_v2 = 'T16FW' ) INTO TABLE et_return.
      ELSE.

        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '031'
                                            iv_langu      = iv_langu
                                            iv_message_v1 = sy-sysid
                                            iv_message_v2 = 'T16FW' ) INTO TABLE et_return.


      ENDIF.
    ENDIF.
  ENDIF.

  " Si no hay errores se continua con la siguiente tabla
  IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_success ] ).
    " Tabla de requisitos de liberación
    IF it_t16fd IS NOT INITIAL.
      MODIFY t16fd FROM TABLE it_t16fd.
      IF sy-subrc = 0.
        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                    iv_id         = zif_rel_data=>cs_msg-id
                                    iv_number     = '032'
                                    iv_langu      = iv_langu
                                    iv_message_v1 = sy-sysid
                                    iv_message_v2 = 'T16FD' ) INTO TABLE et_return.
      ELSE.

        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '031'
                                            iv_langu      = iv_langu
                                            iv_message_v1 = sy-sysid
                                            iv_message_v2 = 'T16FD' ) INTO TABLE et_return.


      ENDIF.
    ENDIF.
  ENDIF.

  " Si no hay errores se continua con la siguiente tabla
  IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_success ] ).
    " Tabla de requisitos de liberación
    IF it_t16fc IS NOT INITIAL.
      MODIFY t16fc FROM TABLE it_t16fc.
      IF sy-subrc = 0.
        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_success
                                    iv_id         = zif_rel_data=>cs_msg-id
                                    iv_number     = '032'
                                    iv_langu      = iv_langu
                                    iv_message_v1 = sy-sysid
                                    iv_message_v2 = 'T16FC' ) INTO TABLE et_return.
      ELSE.

        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '031'
                                            iv_langu      = iv_langu
                                            iv_message_v1 = sy-sysid
                                            iv_message_v2 = 'T16FC' ) INTO TABLE et_return.


      ENDIF.
    ENDIF.
  ENDIF.

  " Borro posibles duplicados
  SORT et_return.
  DELETE ADJACENT DUPLICATES FROM et_return COMPARING ALL FIELDS.

  IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_error ] ).
    IF iv_commit = abap_true.
      ROLLBACK WORK.
    ENDIF.
  ELSEIF iv_commit = abap_true.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
