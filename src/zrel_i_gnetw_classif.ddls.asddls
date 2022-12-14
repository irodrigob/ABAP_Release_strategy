@AbapCatalog.sqlViewName: 'zrel_ddl_003'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Devuelve el valor del importe'
define view ZREL_I_GNETW_CLASSIF
  as select from    ausp
    left outer join zrel_t000 as attin_ekgrp on
    attin_ekgrp.constant = 'CHARACT_ATINN_GNETW'
    left outer join zrel_t000 as attin_klart on
    attin_klart.constant = 'CHARACT_KLART' {
  ausp.objek,
  cast(substring(ausp.objek, 1, 2) as frggr ) as group_lib,
  cast(substring(ausp.objek, 3, 4) as frgsx ) as strategy, 
  atflv as value_from,
  atflb as value_to,
  atcod as value_relation
}
where
  ausp.atinn = attin_ekgrp.value and
  ausp.klart = attin_klart.value
