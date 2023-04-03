# EXCEL EXPORT

Hey,

Here's two examples of Excel's export in csv format, the first is to export the file on a local directory and the second on SAP's Server. 

More than that, there is a logic to divide the files with its maximum capacity of 500001 lines. 

The tvarvs used in both codes are directory addresses.  

 

     *&---------------------------------------------------------------------*  
        *& Form ZF_EXPORTAR_DETALHES  
        *&---------------------------------------------------------------------*  
        * Exporta em arquivo excel para diretório local
        * Excel export to local directory
        *----------------------------------------------------------------------*  
        FORM zf_exportar_detalhes.  
        DATA: vl_filename TYPE string,  
        vl_fullpath TYPE string,  
        vl_name TYPE string,  
        itab_temp TYPE TABLE OF y_ztbfi_cs_docir,  
        itab_temp1 TYPE truxs_t_text_data,  
        wa_temp TYPE y_ztbfi_cs_docir,  
        lv_count TYPE n.  
          
        IF t_ztbfi_cs_docir[] IS NOT INITIAL.  
          
        " Recuperando tvarv e nomenando o arquivo  
        DATA(vl_path) = zcl_tvarv=>get_single_value( 'ZGLOBR_PATH' ).  
        DATA(vl_lines) = lines( t_ztbfi_cs_docir[] ).  
        DATA(wa_title) = 'Empresa;Ano;Matrícula;N° Documento;N° Item;Parceiro;Moeda;Valor;Data do Documento;N° NFe;Série;CFOP;Grupo mercadorias;Local negócios;Utilização;'.
        
        IF vl_lines > 500000.  
          
        DATA(index1) = 1.  
        lv_count = 1 .  
        DATA(index2) = 1.  
          
        DO.  
        index1 = index2.  
        index2 = index1 + 500000.  
        APPEND LINES OF t_ztbfi_cs_docir[] FROM index1 TO index2 TO itab_temp.  
          
        IF itab_temp[] IS INITIAL.  
        EXIT.  
        ENDIF.  
          
        "Formatando nome dos arquivos com cuidado para que não seja sobreescrevido 
        vl_name = | EXCEL_DETALHES_CAPITAL_SOCIAL |.  
        CONCATENATE vl_path vl_name sy-datum sy-uzeit lv_count '.csv' INTO vl_filename SEPARATED BY '_'.  
        CONDENSE vl_filename NO-GAPS.  
          
        IF vl_filename IS NOT INITIAL.  
          
        CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'  
        EXPORTING  
        i_field_seperator = ','  
        TABLES  
        i_tab_sap_data = itab_temp  
        CHANGING  
        i_tab_converted_data = itab_temp1  
        EXCEPTIONS  
        conversion_failed = 1  
        OTHERS = 2.  
        IF sy-subrc <> 0.  
        MESSAGE 'Erro ao converter dados para formato csv.' TYPE c_erro DISPLAY LIKE c_erro.  
        ENDIF.  
        
         " excel header / titles
         INSERT wa_title INTO itab_temp1 INDEX 1.
          
        " Export do excel  
        CALL FUNCTION 'GUI_DOWNLOAD'  
        EXPORTING  
        filename = vl_filename  
        TABLES  
        data_tab = itab_temp1  
        EXCEPTIONS  
        OTHERS = 1.  
          
        IF sy-subrc IS NOT INITIAL.  
        MESSAGE TEXT-067 TYPE c_sucesso DISPLAY LIKE c_erro. "Não foi possível gerar arquivo  
        EXIT.  
        ENDIF.  
          
        ENDIF.  
        lv_count = lv_count + 1.  
        REFRESH: itab_temp, itab_temp1. " Process all the records of itab_temp.  
        ENDDO.  
        ELSE.  
          
          
        " Formatando nome do arquivo com cuidado para que não seja sobreescrito
        vl_name = | EXCEL_DETALHES_CAPITAL_SOCIAL |.  
        CONCATENATE vl_path vl_name sy-datum sy-uzeit '.csv' INTO vl_filename SEPARATED BY '_'.  
        CONDENSE vl_filename NO-GAPS.  
          
        IF vl_filename IS NOT INITIAL.  
          
        CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'  
        EXPORTING  
        i_field_seperator = ','  
        TABLES  
        i_tab_sap_data = t_ztbfi_cs_docir[]  
        CHANGING  
        i_tab_converted_data = itab_temp1  
        EXCEPTIONS  
        conversion_failed = 1  
        OTHERS = 2.  
        IF sy-subrc <> 0.  
        MESSAGE 'Erro ao converter dados para formato csv.' TYPE c_erro DISPLAY LIKE c_erro.  
        ENDIF.  
        
         " excel header / titles
         INSERT wa_title INTO itab_temp1 INDEX 1.
          
        " Export do excel  
        CALL FUNCTION 'GUI_DOWNLOAD'  
        EXPORTING  
        filename = vl_filename  
        TABLES  
        data_tab = itab_temp1  
        EXCEPTIONS  
        OTHERS = 1.  
          
        IF sy-subrc IS NOT INITIAL.  
        MESSAGE TEXT-067 TYPE c_sucesso DISPLAY LIKE c_erro. "Não foi possível gerar arquivo  
        ELSE.  
        CONCATENATE 'Detalhes foram salvos em ' vl_path INTO DATA(vl_message).  
        MESSAGE vl_message TYPE c_sucesso.  
        ENDIF.  
          
        ENDIF.  
        ENDIF.  
        ENDIF.  
          
        ENDFORM.  


In case you need to save the file in the SAP Server, you are going to access transaction **AL11** and select the appropriate directory, you must copy the exact path and respect it's case sensitive form, otherwise you are likely to confront errors. 

After the code runs you can verify the directory chosen and sort it to find your file, to download the data, select the line and copy its name strictly. Access transaction **CG3Y** and fill the information gaps. 

    *&---------------------------------------------------------------------*  
    *& Form zf_exportar_detalhes_bkg  
    *&---------------------------------------------------------------------*  
    *& Exporta em arquivo excel no SERVIDOR 
    *& Excel export to SAP Server
    *&---------------------------------------------------------------------*  
    FORM zf_exportar_detalhes_bkg .  
    DATA: vl_filename TYPE string,  
    vl_name TYPE string,  
    itab_temp TYPE TABLE OF y_ztbfi_cs_docir,  
    itab_temp1 TYPE truxs_t_text_data,  
    wa_temp TYPE y_ztbfi_cs_docir,  
    wa_temp1 TYPE LINE OF truxs_t_text_data,  
    lv_count TYPE n.  
      
    IF t_ztbfi_cs_docir[] IS NOT INITIAL.  
      
    " Recuperando tvarv e nomenando o arquivo  
    DATA(vl_path_bkg) = zcl_tvarv=>get_single_value( 'ZGLOBR_PATH_BKG' ).  
    DATA(vl_lines) = lines( t_ztbfi_cs_docir[] ).  
    DATA(wa_title) = 'Empresa;Ano;Matrícula;N° Documento;N° Item;Parceiro;Moeda;Valor;Data do Documento;N° NFe;Série;CFOP;Grupo mercadorias;Local negócios;Utilização;'.
      
    " Caso mais de 500mil registros, fazer o download dos dados  
    " em vários exceis de no máximo 500 mil registros.  
    IF vl_lines > 500000.  
      
    DATA(index1) = 1.  
    lv_count = 1 .  
    DATA(index2) = 1.  
      
    DO.  
    index1 = index2.  
    index2 = index1 + 500000.  
    APPEND LINES OF t_ztbfi_cs_docir[] FROM index1 TO index2 TO itab_temp.  
      
    IF itab_temp[] IS INITIAL.  
    EXIT.  
    ENDIF.  
      
    "Formatando nome dos arquivos  
    vl_name = | EXCEL_DETALHES_CAPITAL_SOCIAL |.  
    CONCATENATE vl_path_bkg vl_name sy-datum sy-uzeit lv_count '.csv' INTO vl_filename SEPARATED BY '_'.  
    CONDENSE vl_filename NO-GAPS.  
      
    IF vl_filename IS NOT INITIAL.  
      
    "FUNÇÃO DOWNLOAD SERVIDOR  
    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'  
    EXPORTING  
    i_field_seperator = ','  
    TABLES  
    i_tab_sap_data = itab_temp  
    CHANGING  
    i_tab_converted_data = itab_temp1  
    EXCEPTIONS  
    conversion_failed = 1  
    OTHERS = 2.  
    IF sy-subrc <> 0.  
    MESSAGE 'Erro ao converter dados para formato csv.' TYPE c_erro DISPLAY LIKE c_erro.  
    ENDIF.
    
    " excel header / titles
    INSERT wa_title INTO itab_temp1 INDEX 1.
      
    OPEN DATASET vl_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.  
      
    LOOP AT itab_temp1 INTO wa_temp1.  
    TRANSFER wa_temp1 TO vl_filename.  
    ENDLOOP.  
      
    CLOSE DATASET vl_filename.  
      
    IF sy-subrc IS NOT INITIAL.  
    MESSAGE TEXT-067 TYPE c_sucesso DISPLAY LIKE c_erro. "Não foi possível gerar arquivo  
    EXIT.  
    ENDIF.  
      
    ENDIF.  
    lv_count = lv_count + 1.  
    REFRESH: itab_temp, itab_temp1.  
    ENDDO.  
    ELSE. "menos de 500mil registros, realiza o download direto  
      
    " Formatando nome do arquivo  
    vl_name = | EXCEL_DETALHES_CAPITAL_SOCIAL |.  
    CONCATENATE vl_path_bkg vl_name sy-datum sy-uzeit '.csv' INTO vl_filename SEPARATED BY '_'.  
    CONDENSE vl_filename NO-GAPS.  
      
    IF vl_filename IS NOT INITIAL.  
    "FUNÇÃO DOWNLOAD SERVIDOR  
    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'  
    EXPORTING  
    i_field_seperator = ','  
    TABLES  
    i_tab_sap_data = itab_temp  
    CHANGING  
    i_tab_converted_data = itab_temp1  
    EXCEPTIONS  
    conversion_failed = 1  
    OTHERS = 2.  
    IF sy-subrc <> 0.  
    MESSAGE 'Erro ao converter dados para formato csv.' TYPE c_erro DISPLAY LIKE c_erro.  
    ENDIF.  
      
    OPEN DATASET vl_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.  
      
    LOOP AT itab_temp1 INTO wa_temp1.  
    TRANSFER wa_temp1 TO vl_filename.  
    ENDLOOP.  
      
    CLOSE DATASET vl_filename.  
      
    IF sy-subrc IS NOT INITIAL.  
    MESSAGE TEXT-067 TYPE c_sucesso DISPLAY LIKE c_erro. "Não foi possível gerar arquivo  
    EXIT.  
    ENDIF.  
    ENDIF.  
    ENDIF.  
    ENDIF.  
      
    ENDFORM.  
 
