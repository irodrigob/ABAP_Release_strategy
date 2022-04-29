*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZREL_T002.......................................*
DATA:  BEGIN OF STATUS_ZREL_T002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T002                     .
CONTROLS: TCTRL_ZREL_T002
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZREL_T003.......................................*
DATA:  BEGIN OF STATUS_ZREL_T003                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T003                     .
CONTROLS: TCTRL_ZREL_T003
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZREL_T005.......................................*
DATA:  BEGIN OF STATUS_ZREL_T005                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T005                     .
CONTROLS: TCTRL_ZREL_T005
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZREL_T007.......................................*
DATA:  BEGIN OF STATUS_ZREL_T007                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T007                     .
CONTROLS: TCTRL_ZREL_T007
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZREL_T008.......................................*
DATA:  BEGIN OF STATUS_ZREL_T008                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T008                     .
CONTROLS: TCTRL_ZREL_T008
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZREL_T016.......................................*
DATA:  BEGIN OF STATUS_ZREL_T016                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T016                     .
CONTROLS: TCTRL_ZREL_T016
            TYPE TABLEVIEW USING SCREEN '0012'.
*...processing: ZREL_T023.......................................*
DATA:  BEGIN OF STATUS_ZREL_T023                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T023                     .
CONTROLS: TCTRL_ZREL_T023
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZREL_T024.......................................*
DATA:  BEGIN OF STATUS_ZREL_T024                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T024                     .
CONTROLS: TCTRL_ZREL_T024
            TYPE TABLEVIEW USING SCREEN '0013'.
*...processing: ZREL_T025.......................................*
DATA:  BEGIN OF STATUS_ZREL_T025                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T025                     .
CONTROLS: TCTRL_ZREL_T025
            TYPE TABLEVIEW USING SCREEN '0014'.
*...processing: ZREL_T026.......................................*
DATA:  BEGIN OF STATUS_ZREL_T026                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREL_T026                     .
CONTROLS: TCTRL_ZREL_T026
            TYPE TABLEVIEW USING SCREEN '0015'.
*...processing: ZREL_V001.......................................*
TABLES: ZREL_V001, *ZREL_V001. "view work areas
CONTROLS: TCTRL_ZREL_V001
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZREL_V001. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZREL_V001.
* Table for entries selected to show on screen
DATA: BEGIN OF ZREL_V001_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZREL_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V001_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZREL_V001_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZREL_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V001_TOTAL.

*...processing: ZREL_V002.......................................*
TABLES: ZREL_V002, *ZREL_V002. "view work areas
CONTROLS: TCTRL_ZREL_V002
TYPE TABLEVIEW USING SCREEN '0004'.
DATA: BEGIN OF STATUS_ZREL_V002. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZREL_V002.
* Table for entries selected to show on screen
DATA: BEGIN OF ZREL_V002_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZREL_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V002_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZREL_V002_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZREL_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V002_TOTAL.

*...processing: ZREL_V003.......................................*
TABLES: ZREL_V003, *ZREL_V003. "view work areas
CONTROLS: TCTRL_ZREL_V003
TYPE TABLEVIEW USING SCREEN '0005'.
DATA: BEGIN OF STATUS_ZREL_V003. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZREL_V003.
* Table for entries selected to show on screen
DATA: BEGIN OF ZREL_V003_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZREL_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V003_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZREL_V003_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZREL_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V003_TOTAL.

*...processing: ZREL_V004.......................................*
TABLES: ZREL_V004, *ZREL_V004. "view work areas
CONTROLS: TCTRL_ZREL_V004
TYPE TABLEVIEW USING SCREEN '0008'.
DATA: BEGIN OF STATUS_ZREL_V004. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZREL_V004.
* Table for entries selected to show on screen
DATA: BEGIN OF ZREL_V004_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZREL_V004.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V004_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZREL_V004_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZREL_V004.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V004_TOTAL.

*...processing: ZREL_V005.......................................*
TABLES: ZREL_V005, *ZREL_V005. "view work areas
CONTROLS: TCTRL_ZREL_V005
TYPE TABLEVIEW USING SCREEN '0010'.
DATA: BEGIN OF STATUS_ZREL_V005. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZREL_V005.
* Table for entries selected to show on screen
DATA: BEGIN OF ZREL_V005_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZREL_V005.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V005_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZREL_V005_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZREL_V005.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZREL_V005_TOTAL.

*.........table declarations:.................................*
TABLES: *ZREL_T002                     .
TABLES: *ZREL_T002T                    .
TABLES: *ZREL_T003                     .
TABLES: *ZREL_T005                     .
TABLES: *ZREL_T007                     .
TABLES: *ZREL_T007T                    .
TABLES: *ZREL_T008                     .
TABLES: *ZREL_T008T                    .
TABLES: *ZREL_T016                     .
TABLES: *ZREL_T023                     .
TABLES: *ZREL_T024                     .
TABLES: *ZREL_T025                     .
TABLES: *ZREL_T026                     .
TABLES: T024                           .
TABLES: ZREL_T002                      .
TABLES: ZREL_T002T                     .
TABLES: ZREL_T003                      .
TABLES: ZREL_T005                      .
TABLES: ZREL_T006                      .
TABLES: ZREL_T007                      .
TABLES: ZREL_T007T                     .
TABLES: ZREL_T008                      .
TABLES: ZREL_T008T                     .
TABLES: ZREL_T009                      .
TABLES: ZREL_T016                      .
TABLES: ZREL_T023                      .
TABLES: ZREL_T024                      .
TABLES: ZREL_T025                      .
TABLES: ZREL_T026                      .
TABLES: ZZT_CA_CONSTANTS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
