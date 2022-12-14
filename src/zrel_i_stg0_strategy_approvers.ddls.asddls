@AbapCatalog.sqlViewName: 'zrel_ddl_0004'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Aprobador por estrategia'
define view ZREL_I_STG0_STRATEGY_APPROVERS
  as select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 1 as abap.int1 ) as strategy_level,
  frgc1 as approver_code
}
where
  frgc1 <> ''
union select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 2 as abap.int1 ) as strategy_level,
  frgc2 as approver_code
}
where
  frgc2 <> ''
union select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 3 as abap.int1 ) as strategy_level,
  frgc3 as approver_code
}
where
  frgc3 <> ''
union select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 4 as abap.int1 ) as strategy_level,
  frgc4 as approver_code
}
where
  frgc4 <> ''
union select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 5 as abap.int1 ) as strategy_level,
  frgc5 as approver_code
}
where
  frgc5 <> ''
union select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 6 as abap.int1 ) as strategy_level,
  frgc6 as approver_code
}
where
  frgc6 <> ''
union select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 7 as abap.int1 ) as strategy_level,
  frgc7 as approver_code
}
where
  frgc7 <> ''
union select from t16fs {
  frggr as group_lib,
  frgsx as strategy,
  cast( 8 as abap.int1 ) as strategy_level,
  frgc8 as approver_code
}
where
  frgc8 <> ''
