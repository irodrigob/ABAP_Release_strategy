@AbapCatalog.sqlViewName: 'zrel_ddl_0006'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Usuarios aprobadores por estrategia'
define view ZREL_I_STG1_STRATEGY_APPROVERS
  with parameters
  @Environment.systemField: #SYSTEM_LANGUAGE
    langu : spras
  as select from    ZREL_I_STG0_STRATEGY_APPROVERS
    left outer join zrel_i_code_approv_username(langu:$parameters.langu)            on
    ZREL_I_STG0_STRATEGY_APPROVERS.approver_code = zrel_i_code_approv_username.code and
    ZREL_I_STG0_STRATEGY_APPROVERS.group_lib     = zrel_i_code_approv_username.group_lib {
  ZREL_I_STG0_STRATEGY_APPROVERS.group_lib,
  ZREL_I_STG0_STRATEGY_APPROVERS.strategy,
  ZREL_I_STG0_STRATEGY_APPROVERS.strategy_level,
  ZREL_I_STG0_STRATEGY_APPROVERS.approver_code,
  zrel_i_code_approv_username.username,
  zrel_i_code_approv_username.username_Desc
}
