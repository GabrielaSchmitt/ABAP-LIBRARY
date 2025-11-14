* declarar var
DATA(ls_partner) = VALUE ts_partner( partner = '3' name = 'Partner Three' ).

* declarar constante
FINAL(ls_partner) = VALUE ts_partner( partner = '3' name = 'Partner Three' ).

* ternario 
<lf_contract>-st_contrato = COND #( WHEN lt_tab-var = 01 THEN 'FECHADO' ELSE 'ABERTO' ).

* concatenação 
DATA(message) = |Received HTTP code { status_code } with message { text }|.
<lf_contract>-dt_emissao = |{ lw_ekko-aedat(4) }-{ lw_ekko-aedat+4(2) }-{ lw_ekko-aedat+6(2) }|.

* remover/adicionar zeros a esquerda/direita - antes CONVERSION_EXIT_ALPHA_INPUT
 lw_var = condense( |{ iw_var-field ALPHA = OUT }| ).
 lw_var = condense( |{ iw_var-field ALPHA = IN }| ).

@data(lw_ekko) = COND #( WHEN LINE_EXISTS( lt_ekko [ EBELN = '100000001' ] THEN lt_ekko [ EBELN = '100000001' ] ).

* replace - esta removendo espaços da string lr_lote e depois concatenando 
data(lr_item) = 'item2'.
data(lr_lote) = 'lo t 3 e '.
data(lr_novo) = |{ lr_item } Lote: { replace( val = |{ lr_lote ALPHA = OUT }|  sub = ` ` with = '' occ = 0 ) }|.

* condense
data(lr_novo) = |{ lr_item } Lote: { condense( lr_lote ) }|.

* Concatenação com iteração 
DATA(lv_lotes_text) = REDUCE string( INIT lv_inf TYPE string FOR <lf_lin> IN it_j_1bnflin 
                                              WHERE ( charg IS NOT INITIAL ) NEXT lv_inf = SWITCH string( lv_inf
                                              WHEN space THEN 'Lotes transportados' && | | && <lf_lin>-charg
                                              ELSE lv_inf && | | && <lf_lin>-charg ) ).

* Contador de ocorrências
DATA(lv_status) = REDUCE #( INIT lv_cont TYPE i FOR <fs_sfir> IN lt_sfir_root WHERE
                                    ( lifecycle <> lc_lifecycle_06 AND
                                      lifecycle <> lc_lifecycle_16 )
                                      NEXT lv_cont += 1 ).

* Somatória ( com decimais é necessário tipar a variavél x usando o CONV() )
DATA(lv_total_kwert) = REDUCE BAPI_RMWWR(
                                      INIT x = conv kwert( 0 )
                                      FOR ls_prcde IN lt_prcde
                                      NEXT x = x + ls_prcde-kwert ).

* matnr40 to matnr18
DATA(lv_asnum_sel) = |{ conv matnr18( |{ zdart001-matnr_svc ALPHA = OUT }|  ) ALPHA = IN }|.

* loop 
data(result) = VALUE #( FOR row IN input ( row-text ) ).

* read table 
DATA(line) = VALUE #( values[ name = “A” ] OPTIONAL )

* para verificar se o registro existe, ao invés de READ TABLE ou LOOP AT
IF line_exists( my_table[ key = 'A' ] ).

* select verifies boolean
SELECT SINGLE @abap_true
       FROM scarr
       WHERE carrid = @carrier
       INTO @DATA(lv_exists).
