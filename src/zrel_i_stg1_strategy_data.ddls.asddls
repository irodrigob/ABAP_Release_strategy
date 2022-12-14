@AbapCatalog.sqlViewName: 'zrel_ddl_0010'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'INT - Datos completos estrategias + buyers + dptos '
define view ZREL_I_STG1_STRATEGY_DATA
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    langu : spras
  as select from    ZREL_I_STG0_STRATEGY_DATA(langu:$parameters.langu)
    left outer join ZREL_I_BUYERS on
    ZREL_I_STG0_STRATEGY_DATA.pgroup = ZREL_I_BUYERS.pgroup
    left outer join zrel_i_dptos_pgroup(langu:$parameters.langu) on
    ZREL_I_STG0_STRATEGY_DATA.pgroup = zrel_i_dptos_pgroup.pgroup {
  ZREL_I_STG0_STRATEGY_DATA.pgroup,
  ZREL_I_STG0_STRATEGY_DATA.pgroup_desc,
  ZREL_I_STG0_STRATEGY_DATA.group_lib,
  ZREL_I_STG0_STRATEGY_DATA.strategy,
  ZREL_I_STG0_STRATEGY_DATA.value_from,
  ZREL_I_STG0_STRATEGY_DATA.value_to,
  ZREL_I_STG0_STRATEGY_DATA.value_relation,
  ZREL_I_STG0_STRATEGY_DATA.strategy_level,
  ZREL_I_STG0_STRATEGY_DATA.approver_code,
  ZREL_I_STG0_STRATEGY_DATA.username,
  ZREL_I_STG0_STRATEGY_DATA.username_Desc,
  ZREL_I_BUYERS.buyer,
  ZREL_I_BUYERS.buyer_desc,
  zrel_i_dptos_pgroup.dept_subs,
  zrel_i_dptos_pgroup.dept_subs_desc
}
