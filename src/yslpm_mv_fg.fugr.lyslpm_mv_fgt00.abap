*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: YCRMOAUTOSTATVW.................................*
TABLES: YCRMOAUTOSTATVW, *YCRMOAUTOSTATVW. "view work areas
CONTROLS: TCTRL_YCRMOAUTOSTATVW
TYPE TABLEVIEW USING SCREEN '0085'.
DATA: BEGIN OF STATUS_YCRMOAUTOSTATVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YCRMOAUTOSTATVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YCRMOAUTOSTATVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YCRMOAUTOSTATVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YCRMOAUTOSTATVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YCRMOAUTOSTATVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YCRMOAUTOSTATVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YCRMOAUTOSTATVW_TOTAL.

*...processing: YCRMOSLAESCALVW.................................*
TABLES: YCRMOSLAESCALVW, *YCRMOSLAESCALVW. "view work areas
CONTROLS: TCTRL_YCRMOSLAESCALVW
TYPE TABLEVIEW USING SCREEN '0032'.
DATA: BEGIN OF STATUS_YCRMOSLAESCALVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YCRMOSLAESCALVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YCRMOSLAESCALVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YCRMOSLAESCALVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YCRMOSLAESCALVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YCRMOSLAESCALVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YCRMOSLAESCALVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YCRMOSLAESCALVW_TOTAL.

*...processing: YSLPMCUSTPRODVW.................................*
TABLES: YSLPMCUSTPRODVW, *YSLPMCUSTPRODVW. "view work areas
CONTROLS: TCTRL_YSLPMCUSTPRODVW
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_YSLPMCUSTPRODVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMCUSTPRODVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMCUSTPRODVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMCUSTPRODVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMCUSTPRODVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMCUSTPRODVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMCUSTPRODVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMCUSTPRODVW_TOTAL.

*...processing: YSLPMCUSTSYSTVW.................................*
TABLES: YSLPMCUSTSYSTVW, *YSLPMCUSTSYSTVW. "view work areas
CONTROLS: TCTRL_YSLPMCUSTSYSTVW
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_YSLPMCUSTSYSTVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMCUSTSYSTVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMCUSTSYSTVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMCUSTSYSTVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMCUSTSYSTVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMCUSTSYSTVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMCUSTSYSTVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMCUSTSYSTVW_TOTAL.

*...processing: YSLPMEMAILRULEVW................................*
TABLES: YSLPMEMAILRULEVW, *YSLPMEMAILRULEVW. "view work areas
CONTROLS: TCTRL_YSLPMEMAILRULEVW
TYPE TABLEVIEW USING SCREEN '0003'.
DATA: BEGIN OF STATUS_YSLPMEMAILRULEVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMEMAILRULEVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMEMAILRULEVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMEMAILRULEVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMEMAILRULEVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMEMAILRULEVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMEMAILRULEVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMEMAILRULEVW_TOTAL.

*...processing: YSLPMHTTPHDRVW..................................*
TABLES: YSLPMHTTPHDRVW, *YSLPMHTTPHDRVW. "view work areas
CONTROLS: TCTRL_YSLPMHTTPHDRVW
TYPE TABLEVIEW USING SCREEN '0042'.
DATA: BEGIN OF STATUS_YSLPMHTTPHDRVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMHTTPHDRVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMHTTPHDRVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMHTTPHDRVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMHTTPHDRVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMHTTPHDRVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMHTTPHDRVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMHTTPHDRVW_TOTAL.

*...processing: YSLPMPRFLDTRSVW.................................*
TABLES: YSLPMPRFLDTRSVW, *YSLPMPRFLDTRSVW. "view work areas
CONTROLS: TCTRL_YSLPMPRFLDTRSVW
TYPE TABLEVIEW USING SCREEN '0070'.
DATA: BEGIN OF STATUS_YSLPMPRFLDTRSVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMPRFLDTRSVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMPRFLDTRSVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMPRFLDTRSVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMPRFLDTRSVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMPRFLDTRSVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMPRFLDTRSVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMPRFLDTRSVW_TOTAL.

*...processing: YSLPMPRODATTRVW.................................*
TABLES: YSLPMPRODATTRVW, *YSLPMPRODATTRVW. "view work areas
CONTROLS: TCTRL_YSLPMPRODATTRVW
TYPE TABLEVIEW USING SCREEN '0007'.
DATA: BEGIN OF STATUS_YSLPMPRODATTRVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMPRODATTRVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMPRODATTRVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMPRODATTRVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMPRODATTRVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMPRODATTRVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMPRODATTRVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMPRODATTRVW_TOTAL.

*...processing: YSLPMSETUPVW....................................*
TABLES: YSLPMSETUPVW, *YSLPMSETUPVW. "view work areas
CONTROLS: TCTRL_YSLPMSETUPVW
TYPE TABLEVIEW USING SCREEN '0004'.
DATA: BEGIN OF STATUS_YSLPMSETUPVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMSETUPVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMSETUPVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMSETUPVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMSETUPVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMSETUPVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMSETUPVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMSETUPVW_TOTAL.

*...processing: YSLPMSTATEMAILVW................................*
TABLES: YSLPMSTATEMAILVW, *YSLPMSTATEMAILVW. "view work areas
CONTROLS: TCTRL_YSLPMSTATEMAILVW
TYPE TABLEVIEW USING SCREEN '0005'.
DATA: BEGIN OF STATUS_YSLPMSTATEMAILVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMSTATEMAILVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMSTATEMAILVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMSTATEMAILVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMSTATEMAILVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMSTATEMAILVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMSTATEMAILVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMSTATEMAILVW_TOTAL.

*...processing: YSLPMSTATFLOWVW.................................*
TABLES: YSLPMSTATFLOWVW, *YSLPMSTATFLOWVW. "view work areas
CONTROLS: TCTRL_YSLPMSTATFLOWVW
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_YSLPMSTATFLOWVW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YSLPMSTATFLOWVW.
* Table for entries selected to show on screen
DATA: BEGIN OF YSLPMSTATFLOWVW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YSLPMSTATFLOWVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMSTATFLOWVW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YSLPMSTATFLOWVW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YSLPMSTATFLOWVW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YSLPMSTATFLOWVW_TOTAL.

*.........table declarations:.................................*
TABLES: YCRMO_AUTO_STAT                .
TABLES: YCRMO_SLA_ESCAL                .
TABLES: YSLPM_CUST_PROD                .
TABLES: YSLPM_CUST_SYST                .
TABLES: YSLPM_EMAIL_RULE               .
TABLES: YSLPM_HTTP_HDR                 .
TABLES: YSLPM_PROD_ATTR                .
TABLES: YSLPM_PR_FLD_TRS               .
TABLES: YSLPM_SETUP                    .
TABLES: YSLPM_STAT_EMAIL               .
TABLES: YSLPM_STAT_FLOW                .
