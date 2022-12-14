@AbapCatalog.sqlViewName: 'zrel_ddl_0005'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Usuarios de sistema seleccionables'
define view ZREL_I_SYSTEMS_USER
  as select from    usr02
    inner join      usr21 on
    usr02.bname = usr21.bname
    left outer join adrp on
    usr21.persnumber = adrp.persnumber {
  key usr21.bname as username,
  usr02.gltgb,
  name_text as username_desc
}
where
  usr02.ustyp = 'A'
