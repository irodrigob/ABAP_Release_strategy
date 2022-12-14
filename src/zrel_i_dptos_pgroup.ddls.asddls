@AbapCatalog.sqlViewName: 'zrel_ddl_0011'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'REL - Departamentos y grupos de compra'
define view zrel_i_dptos_pgroup
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    langu : spras
  as select from t024
    left outer join   zrel_t003 on
    t024.ekgrp = zrel_t003.ekgrp 
    left outer join zrel_t002t on
     zrel_t003.dept_subs = zrel_t002t.dept_subs
     and zrel_t002t.spras = $parameters.langu {
    t024.ekgrp as pgroup,
    t024.eknam as pgroup_desc,
    case when zrel_t003.dept_subs is null then 'NO_DEPART'
         else zrel_t003.dept_subs
    end as dept_subs,
    case when zrel_t002t.description is null then 'WO Department'
         else zrel_t002t.description
    end as dept_subs_desc

}
