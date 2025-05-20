<lf_contract>-st_contrato = COND #( WHEN lt_tab-var = 01 THEN 'FECHADO' ELSE 'ABERTO' ).
<lf_contract>-dt_emissao = |{ lw_ekko-aedat(4) }-{ lw_ekko-aedat+4(2) }-{ lw_ekko-aedat+6(2) }|.
