@AbapCatalog.sqlViewName: 'zrel_ddl_0007'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Compradores y nombre'
define view ZREL_I_BUYERS as select from zm014
left outer join ZREL_I_SYSTEMS_USER on
zm014.zusuario = ZREL_I_SYSTEMS_USER.username {
zm014.ekgrp as pgroup,
zm014.zusuario as buyer,
ZREL_I_SYSTEMS_USER.username_desc as buyer_desc    
}
