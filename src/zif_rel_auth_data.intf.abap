INTERFACE zif_rel_auth_data
  PUBLIC .

  CONSTANTS: BEGIN OF cs_actvt_auth_check,
               edit     TYPE activ_auth VALUE '02',
               view     TYPE activ_auth VALUE '03',
               approver TYPE activ_auth VALUE '45',
             END OF cs_actvt_auth_check.

ENDINTERFACE.
