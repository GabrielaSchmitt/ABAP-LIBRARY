# Parameters no intervals
<br>
Even if you use the stadard option "NO INTERVALS" in a select options you still have an issue, because the user will have the option to use it via the box of multiple selection aside. 
<br>
<br>

![image](https://github.com/GabrielaSchmitt/ABAP-LIBRARY/assets/86369677/343a2073-28ab-4ce2-a6c2-7d58cf79d6fb)

<br>
You can disasble those tabs creating a perform as the code bellow. 

<br>
<br>
Here is the declaration. 

```abap
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_werks  FOR j_1bnflin-werks     NO INTERVALS NO-EXTENSION,
                  s_nf     FOR j_1bnfdoc-nfenum    NO INTERVALS MATCHCODE OBJECT j1baf.   

At Selection-screen output.
PERFORM f_no_intervals. 
```
<br>

Perform

```abap
FORM f_no_intervals.
  DATA: wl_retrict  TYPE sscr_ass,
        wl_opt_list TYPE sscr_opt_list.
************************************************************************

  wl_opt_list-name       = 'NOINTERVLS'.
  wl_opt_list-options-eq = 'X'.
  APPEND: wl_opt_list TO tl_restrict-opt_list_tab.
  CLEAR: wl_opt_list.

  wl_retrict-kind    = 'S'.
  wl_retrict-name    = 'S_NF'.
  wl_retrict-sg_main = 'I'.
  wl_retrict-sg_addy = ' '.
  wl_retrict-op_main = 'NOINTERVLS'.
  APPEND: wl_retrict TO tl_restrict-ass_tab.
  CLEAR: wl_retrict.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = tl_restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.



ENDFORM. 
```


<br>

Final output 


![image](https://github.com/GabrielaSchmitt/ABAP-LIBRARY/assets/86369677/060a6bc3-0d33-4e0d-a894-8d53f57af6cb)
