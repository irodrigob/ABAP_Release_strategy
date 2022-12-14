@AbapCatalog.sqlViewName: 'zrel_ddl_0001'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Obtención estrategia segun grupo compras'
define view ZREL_I_STG0_CLASSIF_PGROUP
  as select from    ausp
    left outer join zrel_t000 as attin_ekgrp on
    attin_ekgrp.constant = 'CHARACT_ATINN_EKGRP'
    left outer join zrel_t000 as attin_klart on
    attin_klart.constant = 'CHARACT_KLART' {
  ausp.objek,
  ausp.klart,
  cast(substring(ausp.objek, 1, 2) as frggr ) as group_lib,
  cast(substring(ausp.objek, 3, 4) as frgsx ) as strategy,
  cast( ausp.atwrt as ekgrp ) as pgroup
}
where
  ausp.atinn = attin_ekgrp.value and
  ausp.klart  = attin_klart.value
