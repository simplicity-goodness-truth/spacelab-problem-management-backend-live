*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: YSLPMCUSTPRODVW.................................*
FORM GET_DATA_YSLPMCUSTPRODVW.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM YSLPM_CUST_PROD WHERE
(VIM_WHERETAB) .
    CLEAR YSLPMCUSTPRODVW .
YSLPMCUSTPRODVW-MANDT =
YSLPM_CUST_PROD-MANDT .
YSLPMCUSTPRODVW-CUSTOMERBUSINESSPARTNER =
YSLPM_CUST_PROD-CUSTOMERBUSINESSPARTNER .
YSLPMCUSTPRODVW-PRODUCTID =
YSLPM_CUST_PROD-PRODUCTID .
<VIM_TOTAL_STRUC> = YSLPMCUSTPRODVW.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_YSLPMCUSTPRODVW .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO YSLPMCUSTPRODVW.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_YSLPMCUSTPRODVW-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_CUST_PROD WHERE
  CUSTOMERBUSINESSPARTNER = YSLPMCUSTPRODVW-CUSTOMERBUSINESSPARTNER AND
  PRODUCTID = YSLPMCUSTPRODVW-PRODUCTID .
    IF SY-SUBRC = 0.
    DELETE YSLPM_CUST_PROD .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_CUST_PROD WHERE
  CUSTOMERBUSINESSPARTNER = YSLPMCUSTPRODVW-CUSTOMERBUSINESSPARTNER AND
  PRODUCTID = YSLPMCUSTPRODVW-PRODUCTID .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR YSLPM_CUST_PROD.
    ENDIF.
YSLPM_CUST_PROD-MANDT =
YSLPMCUSTPRODVW-MANDT .
YSLPM_CUST_PROD-CUSTOMERBUSINESSPARTNER =
YSLPMCUSTPRODVW-CUSTOMERBUSINESSPARTNER .
YSLPM_CUST_PROD-PRODUCTID =
YSLPMCUSTPRODVW-PRODUCTID .
    IF SY-SUBRC = 0.
    UPDATE YSLPM_CUST_PROD ##WARN_OK.
    ELSE.
    INSERT YSLPM_CUST_PROD .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_YSLPMCUSTPRODVW-UPD_FLAG,
STATUS_YSLPMCUSTPRODVW-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_YSLPMCUSTPRODVW.
  SELECT SINGLE * FROM YSLPM_CUST_PROD WHERE
CUSTOMERBUSINESSPARTNER = YSLPMCUSTPRODVW-CUSTOMERBUSINESSPARTNER AND
PRODUCTID = YSLPMCUSTPRODVW-PRODUCTID .
YSLPMCUSTPRODVW-MANDT =
YSLPM_CUST_PROD-MANDT .
YSLPMCUSTPRODVW-CUSTOMERBUSINESSPARTNER =
YSLPM_CUST_PROD-CUSTOMERBUSINESSPARTNER .
YSLPMCUSTPRODVW-PRODUCTID =
YSLPM_CUST_PROD-PRODUCTID .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_YSLPMCUSTPRODVW USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE YSLPMCUSTPRODVW-CUSTOMERBUSINESSPARTNER TO
YSLPM_CUST_PROD-CUSTOMERBUSINESSPARTNER .
MOVE YSLPMCUSTPRODVW-PRODUCTID TO
YSLPM_CUST_PROD-PRODUCTID .
MOVE YSLPMCUSTPRODVW-MANDT TO
YSLPM_CUST_PROD-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'YSLPM_CUST_PROD'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN YSLPM_CUST_PROD TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'YSLPM_CUST_PROD'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: YSLPMCUSTSYSTVW.................................*
FORM GET_DATA_YSLPMCUSTSYSTVW.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM YSLPM_CUST_SYST WHERE
(VIM_WHERETAB) .
    CLEAR YSLPMCUSTSYSTVW .
YSLPMCUSTSYSTVW-MANDT =
YSLPM_CUST_SYST-MANDT .
YSLPMCUSTSYSTVW-CUSTOMERBUSINESSPARTNER =
YSLPM_CUST_SYST-CUSTOMERBUSINESSPARTNER .
YSLPMCUSTSYSTVW-SAPSYSTEMNAME =
YSLPM_CUST_SYST-SAPSYSTEMNAME .
YSLPMCUSTSYSTVW-INSTALLATIONNUMBER =
YSLPM_CUST_SYST-INSTALLATIONNUMBER .
YSLPMCUSTSYSTVW-SYSTEMNUMBER =
YSLPM_CUST_SYST-SYSTEMNUMBER .
YSLPMCUSTSYSTVW-DESCRIPTION =
YSLPM_CUST_SYST-DESCRIPTION .
YSLPMCUSTSYSTVW-ROLE =
YSLPM_CUST_SYST-ROLE .
<VIM_TOTAL_STRUC> = YSLPMCUSTSYSTVW.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_YSLPMCUSTSYSTVW .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO YSLPMCUSTSYSTVW.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_YSLPMCUSTSYSTVW-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_CUST_SYST WHERE
  CUSTOMERBUSINESSPARTNER = YSLPMCUSTSYSTVW-CUSTOMERBUSINESSPARTNER AND
  SAPSYSTEMNAME = YSLPMCUSTSYSTVW-SAPSYSTEMNAME .
    IF SY-SUBRC = 0.
    DELETE YSLPM_CUST_SYST .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_CUST_SYST WHERE
  CUSTOMERBUSINESSPARTNER = YSLPMCUSTSYSTVW-CUSTOMERBUSINESSPARTNER AND
  SAPSYSTEMNAME = YSLPMCUSTSYSTVW-SAPSYSTEMNAME .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR YSLPM_CUST_SYST.
    ENDIF.
YSLPM_CUST_SYST-MANDT =
YSLPMCUSTSYSTVW-MANDT .
YSLPM_CUST_SYST-CUSTOMERBUSINESSPARTNER =
YSLPMCUSTSYSTVW-CUSTOMERBUSINESSPARTNER .
YSLPM_CUST_SYST-SAPSYSTEMNAME =
YSLPMCUSTSYSTVW-SAPSYSTEMNAME .
YSLPM_CUST_SYST-INSTALLATIONNUMBER =
YSLPMCUSTSYSTVW-INSTALLATIONNUMBER .
YSLPM_CUST_SYST-SYSTEMNUMBER =
YSLPMCUSTSYSTVW-SYSTEMNUMBER .
YSLPM_CUST_SYST-DESCRIPTION =
YSLPMCUSTSYSTVW-DESCRIPTION .
YSLPM_CUST_SYST-ROLE =
YSLPMCUSTSYSTVW-ROLE .
    IF SY-SUBRC = 0.
    UPDATE YSLPM_CUST_SYST ##WARN_OK.
    ELSE.
    INSERT YSLPM_CUST_SYST .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_YSLPMCUSTSYSTVW-UPD_FLAG,
STATUS_YSLPMCUSTSYSTVW-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_YSLPMCUSTSYSTVW.
  SELECT SINGLE * FROM YSLPM_CUST_SYST WHERE
CUSTOMERBUSINESSPARTNER = YSLPMCUSTSYSTVW-CUSTOMERBUSINESSPARTNER AND
SAPSYSTEMNAME = YSLPMCUSTSYSTVW-SAPSYSTEMNAME .
YSLPMCUSTSYSTVW-MANDT =
YSLPM_CUST_SYST-MANDT .
YSLPMCUSTSYSTVW-CUSTOMERBUSINESSPARTNER =
YSLPM_CUST_SYST-CUSTOMERBUSINESSPARTNER .
YSLPMCUSTSYSTVW-SAPSYSTEMNAME =
YSLPM_CUST_SYST-SAPSYSTEMNAME .
YSLPMCUSTSYSTVW-INSTALLATIONNUMBER =
YSLPM_CUST_SYST-INSTALLATIONNUMBER .
YSLPMCUSTSYSTVW-SYSTEMNUMBER =
YSLPM_CUST_SYST-SYSTEMNUMBER .
YSLPMCUSTSYSTVW-DESCRIPTION =
YSLPM_CUST_SYST-DESCRIPTION .
YSLPMCUSTSYSTVW-ROLE =
YSLPM_CUST_SYST-ROLE .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_YSLPMCUSTSYSTVW USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE YSLPMCUSTSYSTVW-CUSTOMERBUSINESSPARTNER TO
YSLPM_CUST_SYST-CUSTOMERBUSINESSPARTNER .
MOVE YSLPMCUSTSYSTVW-SAPSYSTEMNAME TO
YSLPM_CUST_SYST-SAPSYSTEMNAME .
MOVE YSLPMCUSTSYSTVW-MANDT TO
YSLPM_CUST_SYST-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'YSLPM_CUST_SYST'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN YSLPM_CUST_SYST TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'YSLPM_CUST_SYST'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: YSLPMEMAILRULEVW................................*
FORM GET_DATA_YSLPMEMAILRULEVW.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM YSLPM_EMAIL_RULE WHERE
(VIM_WHERETAB) .
    CLEAR YSLPMEMAILRULEVW .
YSLPMEMAILRULEVW-MANDT =
YSLPM_EMAIL_RULE-MANDT .
YSLPMEMAILRULEVW-RULENAME =
YSLPM_EMAIL_RULE-RULENAME .
YSLPMEMAILRULEVW-INTERNAL =
YSLPM_EMAIL_RULE-INTERNAL .
YSLPMEMAILRULEVW-STTEXTSUBJ =
YSLPM_EMAIL_RULE-STTEXTSUBJ .
YSLPMEMAILRULEVW-STTEXTBODY =
YSLPM_EMAIL_RULE-STTEXTBODY .
<VIM_TOTAL_STRUC> = YSLPMEMAILRULEVW.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_YSLPMEMAILRULEVW .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO YSLPMEMAILRULEVW.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_YSLPMEMAILRULEVW-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_EMAIL_RULE WHERE
  RULENAME = YSLPMEMAILRULEVW-RULENAME .
    IF SY-SUBRC = 0.
    DELETE YSLPM_EMAIL_RULE .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_EMAIL_RULE WHERE
  RULENAME = YSLPMEMAILRULEVW-RULENAME .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR YSLPM_EMAIL_RULE.
    ENDIF.
YSLPM_EMAIL_RULE-MANDT =
YSLPMEMAILRULEVW-MANDT .
YSLPM_EMAIL_RULE-RULENAME =
YSLPMEMAILRULEVW-RULENAME .
YSLPM_EMAIL_RULE-INTERNAL =
YSLPMEMAILRULEVW-INTERNAL .
YSLPM_EMAIL_RULE-STTEXTSUBJ =
YSLPMEMAILRULEVW-STTEXTSUBJ .
YSLPM_EMAIL_RULE-STTEXTBODY =
YSLPMEMAILRULEVW-STTEXTBODY .
    IF SY-SUBRC = 0.
    UPDATE YSLPM_EMAIL_RULE ##WARN_OK.
    ELSE.
    INSERT YSLPM_EMAIL_RULE .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_YSLPMEMAILRULEVW-UPD_FLAG,
STATUS_YSLPMEMAILRULEVW-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_YSLPMEMAILRULEVW.
  SELECT SINGLE * FROM YSLPM_EMAIL_RULE WHERE
RULENAME = YSLPMEMAILRULEVW-RULENAME .
YSLPMEMAILRULEVW-MANDT =
YSLPM_EMAIL_RULE-MANDT .
YSLPMEMAILRULEVW-RULENAME =
YSLPM_EMAIL_RULE-RULENAME .
YSLPMEMAILRULEVW-INTERNAL =
YSLPM_EMAIL_RULE-INTERNAL .
YSLPMEMAILRULEVW-STTEXTSUBJ =
YSLPM_EMAIL_RULE-STTEXTSUBJ .
YSLPMEMAILRULEVW-STTEXTBODY =
YSLPM_EMAIL_RULE-STTEXTBODY .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_YSLPMEMAILRULEVW USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE YSLPMEMAILRULEVW-RULENAME TO
YSLPM_EMAIL_RULE-RULENAME .
MOVE YSLPMEMAILRULEVW-MANDT TO
YSLPM_EMAIL_RULE-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'YSLPM_EMAIL_RULE'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN YSLPM_EMAIL_RULE TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'YSLPM_EMAIL_RULE'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: YSLPMPRODATTRVW.................................*
FORM GET_DATA_YSLPMPRODATTRVW.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM YSLPM_PROD_ATTR WHERE
(VIM_WHERETAB) .
    CLEAR YSLPMPRODATTRVW .
YSLPMPRODATTRVW-MANDT =
YSLPM_PROD_ATTR-MANDT .
YSLPMPRODATTRVW-ID =
YSLPM_PROD_ATTR-ID .
YSLPMPRODATTRVW-DESCRIPTION =
YSLPM_PROD_ATTR-DESCRIPTION .
YSLPMPRODATTRVW-SHOWPRIORITIES =
YSLPM_PROD_ATTR-SHOWPRIORITIES .
<VIM_TOTAL_STRUC> = YSLPMPRODATTRVW.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_YSLPMPRODATTRVW .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO YSLPMPRODATTRVW.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_YSLPMPRODATTRVW-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_PROD_ATTR WHERE
  ID = YSLPMPRODATTRVW-ID .
    IF SY-SUBRC = 0.
    DELETE YSLPM_PROD_ATTR .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_PROD_ATTR WHERE
  ID = YSLPMPRODATTRVW-ID .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR YSLPM_PROD_ATTR.
    ENDIF.
YSLPM_PROD_ATTR-MANDT =
YSLPMPRODATTRVW-MANDT .
YSLPM_PROD_ATTR-ID =
YSLPMPRODATTRVW-ID .
YSLPM_PROD_ATTR-DESCRIPTION =
YSLPMPRODATTRVW-DESCRIPTION .
YSLPM_PROD_ATTR-SHOWPRIORITIES =
YSLPMPRODATTRVW-SHOWPRIORITIES .
    IF SY-SUBRC = 0.
    UPDATE YSLPM_PROD_ATTR ##WARN_OK.
    ELSE.
    INSERT YSLPM_PROD_ATTR .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_YSLPMPRODATTRVW-UPD_FLAG,
STATUS_YSLPMPRODATTRVW-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_YSLPMPRODATTRVW.
  SELECT SINGLE * FROM YSLPM_PROD_ATTR WHERE
ID = YSLPMPRODATTRVW-ID .
YSLPMPRODATTRVW-MANDT =
YSLPM_PROD_ATTR-MANDT .
YSLPMPRODATTRVW-ID =
YSLPM_PROD_ATTR-ID .
YSLPMPRODATTRVW-DESCRIPTION =
YSLPM_PROD_ATTR-DESCRIPTION .
YSLPMPRODATTRVW-SHOWPRIORITIES =
YSLPM_PROD_ATTR-SHOWPRIORITIES .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_YSLPMPRODATTRVW USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE YSLPMPRODATTRVW-ID TO
YSLPM_PROD_ATTR-ID .
MOVE YSLPMPRODATTRVW-MANDT TO
YSLPM_PROD_ATTR-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'YSLPM_PROD_ATTR'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN YSLPM_PROD_ATTR TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'YSLPM_PROD_ATTR'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: YSLPMSETUPVW....................................*
FORM GET_DATA_YSLPMSETUPVW.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM YSLPM_SETUP WHERE
(VIM_WHERETAB) .
    CLEAR YSLPMSETUPVW .
YSLPMSETUPVW-MANDT =
YSLPM_SETUP-MANDT .
YSLPMSETUPVW-PARAM =
YSLPM_SETUP-PARAM .
YSLPMSETUPVW-VALUE =
YSLPM_SETUP-VALUE .
YSLPMSETUPVW-DESCRIPTION =
YSLPM_SETUP-DESCRIPTION .
<VIM_TOTAL_STRUC> = YSLPMSETUPVW.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_YSLPMSETUPVW .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO YSLPMSETUPVW.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_YSLPMSETUPVW-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_SETUP WHERE
  PARAM = YSLPMSETUPVW-PARAM .
    IF SY-SUBRC = 0.
    DELETE YSLPM_SETUP .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_SETUP WHERE
  PARAM = YSLPMSETUPVW-PARAM .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR YSLPM_SETUP.
    ENDIF.
YSLPM_SETUP-MANDT =
YSLPMSETUPVW-MANDT .
YSLPM_SETUP-PARAM =
YSLPMSETUPVW-PARAM .
YSLPM_SETUP-VALUE =
YSLPMSETUPVW-VALUE .
YSLPM_SETUP-DESCRIPTION =
YSLPMSETUPVW-DESCRIPTION .
    IF SY-SUBRC = 0.
    UPDATE YSLPM_SETUP ##WARN_OK.
    ELSE.
    INSERT YSLPM_SETUP .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_YSLPMSETUPVW-UPD_FLAG,
STATUS_YSLPMSETUPVW-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_YSLPMSETUPVW.
  SELECT SINGLE * FROM YSLPM_SETUP WHERE
PARAM = YSLPMSETUPVW-PARAM .
YSLPMSETUPVW-MANDT =
YSLPM_SETUP-MANDT .
YSLPMSETUPVW-PARAM =
YSLPM_SETUP-PARAM .
YSLPMSETUPVW-VALUE =
YSLPM_SETUP-VALUE .
YSLPMSETUPVW-DESCRIPTION =
YSLPM_SETUP-DESCRIPTION .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_YSLPMSETUPVW USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE YSLPMSETUPVW-PARAM TO
YSLPM_SETUP-PARAM .
MOVE YSLPMSETUPVW-MANDT TO
YSLPM_SETUP-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'YSLPM_SETUP'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN YSLPM_SETUP TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'YSLPM_SETUP'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: YSLPMSTATEMAILVW................................*
FORM GET_DATA_YSLPMSTATEMAILVW.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM YSLPM_STAT_EMAIL WHERE
(VIM_WHERETAB) .
    CLEAR YSLPMSTATEMAILVW .
YSLPMSTATEMAILVW-MANDT =
YSLPM_STAT_EMAIL-MANDT .
YSLPMSTATEMAILVW-RULE_ID =
YSLPM_STAT_EMAIL-RULE_ID .
YSLPMSTATEMAILVW-STATUSIN =
YSLPM_STAT_EMAIL-STATUSIN .
YSLPMSTATEMAILVW-STATUSOUT =
YSLPM_STAT_EMAIL-STATUSOUT .
YSLPMSTATEMAILVW-EMAILRULEREQ =
YSLPM_STAT_EMAIL-EMAILRULEREQ .
YSLPMSTATEMAILVW-EMAILRULESUP =
YSLPM_STAT_EMAIL-EMAILRULESUP .
YSLPMSTATEMAILVW-EMAILRULEPRO =
YSLPM_STAT_EMAIL-EMAILRULEPRO .
YSLPMSTATEMAILVW-EMAILRULEOBS =
YSLPM_STAT_EMAIL-EMAILRULEOBS .
<VIM_TOTAL_STRUC> = YSLPMSTATEMAILVW.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_YSLPMSTATEMAILVW .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO YSLPMSTATEMAILVW.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_YSLPMSTATEMAILVW-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_STAT_EMAIL WHERE
  RULE_ID = YSLPMSTATEMAILVW-RULE_ID .
    IF SY-SUBRC = 0.
    DELETE YSLPM_STAT_EMAIL .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_STAT_EMAIL WHERE
  RULE_ID = YSLPMSTATEMAILVW-RULE_ID .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR YSLPM_STAT_EMAIL.
    ENDIF.
YSLPM_STAT_EMAIL-MANDT =
YSLPMSTATEMAILVW-MANDT .
YSLPM_STAT_EMAIL-RULE_ID =
YSLPMSTATEMAILVW-RULE_ID .
YSLPM_STAT_EMAIL-STATUSIN =
YSLPMSTATEMAILVW-STATUSIN .
YSLPM_STAT_EMAIL-STATUSOUT =
YSLPMSTATEMAILVW-STATUSOUT .
YSLPM_STAT_EMAIL-EMAILRULEREQ =
YSLPMSTATEMAILVW-EMAILRULEREQ .
YSLPM_STAT_EMAIL-EMAILRULESUP =
YSLPMSTATEMAILVW-EMAILRULESUP .
YSLPM_STAT_EMAIL-EMAILRULEPRO =
YSLPMSTATEMAILVW-EMAILRULEPRO .
YSLPM_STAT_EMAIL-EMAILRULEOBS =
YSLPMSTATEMAILVW-EMAILRULEOBS .
    IF SY-SUBRC = 0.
    UPDATE YSLPM_STAT_EMAIL ##WARN_OK.
    ELSE.
    INSERT YSLPM_STAT_EMAIL .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_YSLPMSTATEMAILVW-UPD_FLAG,
STATUS_YSLPMSTATEMAILVW-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_YSLPMSTATEMAILVW.
  SELECT SINGLE * FROM YSLPM_STAT_EMAIL WHERE
RULE_ID = YSLPMSTATEMAILVW-RULE_ID .
YSLPMSTATEMAILVW-MANDT =
YSLPM_STAT_EMAIL-MANDT .
YSLPMSTATEMAILVW-RULE_ID =
YSLPM_STAT_EMAIL-RULE_ID .
YSLPMSTATEMAILVW-STATUSIN =
YSLPM_STAT_EMAIL-STATUSIN .
YSLPMSTATEMAILVW-STATUSOUT =
YSLPM_STAT_EMAIL-STATUSOUT .
YSLPMSTATEMAILVW-EMAILRULEREQ =
YSLPM_STAT_EMAIL-EMAILRULEREQ .
YSLPMSTATEMAILVW-EMAILRULESUP =
YSLPM_STAT_EMAIL-EMAILRULESUP .
YSLPMSTATEMAILVW-EMAILRULEPRO =
YSLPM_STAT_EMAIL-EMAILRULEPRO .
YSLPMSTATEMAILVW-EMAILRULEOBS =
YSLPM_STAT_EMAIL-EMAILRULEOBS .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_YSLPMSTATEMAILVW USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE YSLPMSTATEMAILVW-RULE_ID TO
YSLPM_STAT_EMAIL-RULE_ID .
MOVE YSLPMSTATEMAILVW-MANDT TO
YSLPM_STAT_EMAIL-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'YSLPM_STAT_EMAIL'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN YSLPM_STAT_EMAIL TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'YSLPM_STAT_EMAIL'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: YSLPMSTATFLOWVW.................................*
FORM GET_DATA_YSLPMSTATFLOWVW.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM YSLPM_STAT_FLOW WHERE
(VIM_WHERETAB) .
    CLEAR YSLPMSTATFLOWVW .
YSLPMSTATFLOWVW-MANDT =
YSLPM_STAT_FLOW-MANDT .
YSLPMSTATFLOWVW-STSMA =
YSLPM_STAT_FLOW-STSMA .
YSLPMSTATFLOWVW-STATUS =
YSLPM_STAT_FLOW-STATUS .
YSLPMSTATFLOWVW-STATUSLIST =
YSLPM_STAT_FLOW-STATUSLIST .
<VIM_TOTAL_STRUC> = YSLPMSTATFLOWVW.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_YSLPMSTATFLOWVW .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO YSLPMSTATFLOWVW.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_YSLPMSTATFLOWVW-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_STAT_FLOW WHERE
  STSMA = YSLPMSTATFLOWVW-STSMA AND
  STATUS = YSLPMSTATFLOWVW-STATUS .
    IF SY-SUBRC = 0.
    DELETE YSLPM_STAT_FLOW .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM YSLPM_STAT_FLOW WHERE
  STSMA = YSLPMSTATFLOWVW-STSMA AND
  STATUS = YSLPMSTATFLOWVW-STATUS .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR YSLPM_STAT_FLOW.
    ENDIF.
YSLPM_STAT_FLOW-MANDT =
YSLPMSTATFLOWVW-MANDT .
YSLPM_STAT_FLOW-STSMA =
YSLPMSTATFLOWVW-STSMA .
YSLPM_STAT_FLOW-STATUS =
YSLPMSTATFLOWVW-STATUS .
YSLPM_STAT_FLOW-STATUSLIST =
YSLPMSTATFLOWVW-STATUSLIST .
    IF SY-SUBRC = 0.
    UPDATE YSLPM_STAT_FLOW ##WARN_OK.
    ELSE.
    INSERT YSLPM_STAT_FLOW .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_YSLPMSTATFLOWVW-UPD_FLAG,
STATUS_YSLPMSTATFLOWVW-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_YSLPMSTATFLOWVW.
  SELECT SINGLE * FROM YSLPM_STAT_FLOW WHERE
STSMA = YSLPMSTATFLOWVW-STSMA AND
STATUS = YSLPMSTATFLOWVW-STATUS .
YSLPMSTATFLOWVW-MANDT =
YSLPM_STAT_FLOW-MANDT .
YSLPMSTATFLOWVW-STSMA =
YSLPM_STAT_FLOW-STSMA .
YSLPMSTATFLOWVW-STATUS =
YSLPM_STAT_FLOW-STATUS .
YSLPMSTATFLOWVW-STATUSLIST =
YSLPM_STAT_FLOW-STATUSLIST .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_YSLPMSTATFLOWVW USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE YSLPMSTATFLOWVW-STSMA TO
YSLPM_STAT_FLOW-STSMA .
MOVE YSLPMSTATFLOWVW-STATUS TO
YSLPM_STAT_FLOW-STATUS .
MOVE YSLPMSTATFLOWVW-MANDT TO
YSLPM_STAT_FLOW-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'YSLPM_STAT_FLOW'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN YSLPM_STAT_FLOW TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'YSLPM_STAT_FLOW'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
