@AbapCatalog.sqlViewName: 'zrel_ddl_0012'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Devuelve usuario sap de los codigos de aprobacion'
define view zrel_i_code_approv_username
  with parameters
    langu : spras
  as select from    t16fw
    left outer join t16fd as description_system_language                  on
    t16fw.frggr                       = description_system_language.frggr and
    t16fw.frgco                       = description_system_language.frgco and
    description_system_language.spras = $parameters.langu
    left outer join t16fd as description_english          on
    t16fw.frggr               = description_english.frggr and
    t16fw.frgco               = description_english.frgco and
    description_english.spras = 'E'
    left outer join ZREL_I_SYSTEMS_USER on
    t16fw.objid = ZREL_I_SYSTEMS_USER.username {
  t16fw.frggr as group_lib,
  t16fw.frgco as code,
  t16fw.objid as username,
   case
    when username_desc <> '' then username_desc 
    when description_system_language.frgct <> '' then description_system_language.frgct
    when description_english.frgct <> '' then description_english.frgct
    else username
  end as username_Desc
}
