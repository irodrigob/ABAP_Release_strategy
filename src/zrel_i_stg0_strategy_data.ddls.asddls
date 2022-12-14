@AbapCatalog.sqlViewName: 'zrel_ddl_0008'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'INT - Datos completos estrategias'
define view ZREL_I_STG0_STRATEGY_DATA
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    langu : spras
  as select from    ZREL_I_STRATEGY_AMOUNTS
    left outer join ZREL_I_STG1_STRATEGY_APPROVERS(langu:$parameters.langu)      on
    ZREL_I_STRATEGY_AMOUNTS.group_lib = ZREL_I_STG1_STRATEGY_APPROVERS.group_lib and
    ZREL_I_STRATEGY_AMOUNTS.strategy  = ZREL_I_STG1_STRATEGY_APPROVERS.strategy {
  ZREL_I_STRATEGY_AMOUNTS.pgroup,
  ZREL_I_STRATEGY_AMOUNTS.pgroup_desc,
  ZREL_I_STRATEGY_AMOUNTS.group_lib,
  ZREL_I_STRATEGY_AMOUNTS.strategy,
  ZREL_I_STRATEGY_AMOUNTS.value_from,
  ZREL_I_STRATEGY_AMOUNTS.value_to,
  ZREL_I_STRATEGY_AMOUNTS.value_relation,
  ZREL_I_STG1_STRATEGY_APPROVERS.strategy_level,
  ZREL_I_STG1_STRATEGY_APPROVERS.approver_code,
  ZREL_I_STG1_STRATEGY_APPROVERS.username,
  ZREL_I_STG1_STRATEGY_APPROVERS.username_Desc
}
