
In Brazil a company's registration number is called CNPJ (“**C**adastro **N**acional de **P**essoas **J**urídicas”).

Here's how to contruct a CNPJ on SAP using ABAP language. 

You need two main informations, company (BUKRS) and business place (BUPLA/BRANCH). If counfused please take a look into the header structure of both functions used bellow. 


    DATA: cgc_company TYPE j_1bwfield-cgc_compan,  
	      cgc_branch TYPE j_1bwfield-cgc_branch.

      CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
        EXPORTING
          bukrs       = bukrs
        IMPORTING
          cgc_company = cgc_company.
    
      CALL FUNCTION 'J_1BBUILD_CGC'
        EXPORTING
          cgc_company = cgc_company
          cgc_branch  = branch_data-cgc_branch
        IMPORTING
          cgc_number  = cgc_number.
	  IF sy-subrc <> 0.  
	    "MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.  
	    RETURN.  
	  ENDIF.  
	  
	  " CNPJ formatting options 
	  DATA lv_creditor_taxnum TYPE c LENGTH 18.  
	  CLEAR lv_creditor_taxnum.  
	  WRITE lv_cgc USING EDIT MASK '__.___.___/____-__' TO lv_creditor_taxnum.

Here's an example of how to get the Main Branch:

    data l_bupla  TYPE j_1bbranch-branch. 
    
    SELECT SINGLE branch  
    INTO @l_bupla  
    FROM j_1bbranch  
    WHERE bukrs EQ @bukrs  
    AND branch EQ '0001'.
