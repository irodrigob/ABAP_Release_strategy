@AbapCatalog.sqlViewName: 'zrel_ddl_0009'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Importes de las estrategias'
define view ZREL_I_STRATEGY_AMOUNTS
  as select from    ZREL_I_STG0_CLASSIF_PGROUP
    left outer join ZREL_I_GNETW_CLASSIF on
    ZREL_I_STG0_CLASSIF_PGROUP.objek = ZREL_I_GNETW_CLASSIF.objek
    left outer join t024 as pgroup_desc on
    ZREL_I_STG0_CLASSIF_PGROUP.pgroup = pgroup_desc.ekgrp {
  ZREL_I_STG0_CLASSIF_PGROUP.pgroup,
  ZREL_I_STG0_CLASSIF_PGROUP.group_lib,
  ZREL_I_STG0_CLASSIF_PGROUP.strategy,  
  ZREL_I_GNETW_CLASSIF.value_from,
  ZREL_I_GNETW_CLASSIF.value_to,
  ZREL_I_GNETW_CLASSIF.value_relation,
  pgroup_desc.eknam as pgroup_desc
}
