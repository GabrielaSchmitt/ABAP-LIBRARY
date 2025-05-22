* ternario 
<lf_contract>-st_contrato = COND #( WHEN lt_tab-var = 01 THEN 'FECHADO' ELSE 'ABERTO' ).

* concatenação 
<lf_contract>-dt_emissao = |{ lw_ekko-aedat(4) }-{ lw_ekko-aedat+4(2) }-{ lw_ekko-aedat+6(2) }|.

* remover/adicionar zeros a esquerda/direita - antes CONVERSION_EXIT_ALPHA_INPUT
 lw_var = condense( |{ iw_var-field ALPHA = OUT }| ).
 lw_var = condense( |{ iw_var-field ALPHA = IN }| ).

@data(lw_ekko) = COND #( WHEN LINE_EXISTS( lt_ekko [ EBELN = '100000001' ] THEN lt_ekko [ EBELN = '100000001' ] ).
