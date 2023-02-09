# POP UP

There are many POP UPs function modules, such as the above listed:

-   POPUP_WITH_TABLE_DISPLAY
-   POPUP_TO_CONFIRM_STEP
-   POPUP_TO_DECIDE_WITH_MESSAGE
-   POPUP_TO_DECIDE
-   POPUP_TO_SELECT_MONTH
-   POPUP_TO_CONFIRM_WITH_VALUE
-   POPUP_TO_CONFIRM_WITH_MESSAGE
-   POPUP_TO_DISPLAY_TEXT
-   POPUP_TO_CONFIRM
-   POPUP_TO_CONTINUE_YES_NO
-   POPUP_TO_CONFIRM_DATA_LOSS

Here's an example of pop up to display information to the user,  you display a message, they confirm and its supposed to be back at the last transaction to change a wrong information filled. 

    DATA: lv_numitem TYPE i,  
    lv_qtd TYPE i,  
    lv_qtd_dis TYPE i.  
      
    lv_numitem = 5.  
    lv_qtd = 30.  
    lv_qtd_dis = 10.  
      
    ***POP UP TEXT DISPLAY EXAMPLE***  
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'  
    EXPORTING  
    titel = 'Verificar quantidade, documento já possui saídas com ZOEF.'  
    textline1 = | N° item: | & |{ lv_numitem }|  
    textline2 = | Quantidade informada: | & |{ lv_qtd }|  
    textline3 = | Quantidade disponível: | & |{ lv_qtd_dis }|  
    start_row = 6.  
    
    STOP.  
    MESSAGE | Está disponível a quantidade de | & |{ lv_qtd_dis }| TYPE 'E'.

![image](https://user-images.githubusercontent.com/86369677/217902928-7d48b83f-55c0-4c0e-a18a-2a868302e641.png)

refs and usefull links: 
 - https://wiki.scn.sap.com/wiki/display/ABAP/Different+Pop_Ups+in+ABAP
 - http://abapfox.blogspot.com/2016/01/popup-textos-quais-as-funcoes-mais.html



