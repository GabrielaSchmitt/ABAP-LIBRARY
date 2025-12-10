# Tvarvs

**TVARV** is a standard Logical DB, Selection Screens, Selection Variants Transparent Table in SAP Basis application, which stores Table of variables in selection criteria data.

You can access all tvarvs of the ambient in the transaction stvarv, or else go to se11 or se16n and search for the table TVARVC. 

![image](https://user-images.githubusercontent.com/86369677/217553710-1e4600e1-7b3d-4cd5-ad50-513ebe6242d9.png)

Create your tvarv on the transaction stvarv, it is simple. Select the option modify and then create ![image](https://user-images.githubusercontent.com/86369677/217553872-7531bcf8-aac3-42e9-9a9e-e69c8ed5fcfd.png)
.

Notice that you have two options, tvarv of parameter or tvarv Select Options. The parameter one is used to store a simple value. In case you need multiple values, use the Select Options. 

![image](https://user-images.githubusercontent.com/86369677/217554020-5e0f3c93-ee76-4a29-afdc-d9e2ee212379.png)


To create, on the left you should give it a name, a best practice is to start with Z as it is not a standard development. And on the right side you can enter the content needed. Observe that if you want it to be case sensitive you need to check the checkbox aside. 

![image](https://user-images.githubusercontent.com/86369677/217554135-2ed3b7c4-287f-453d-9279-dfb6ce079a1b.png)

On the Select Options there are more columns. 

Pay attention to the checkbox on the top of the page if you want to include those changings into a transport request. 
![image](https://user-images.githubusercontent.com/86369677/217554179-d72c3903-2c28-43e5-bff7-507f0ea389fb.png)

After creating you simply click on save and its done. ![image](https://user-images.githubusercontent.com/86369677/217554249-1c050602-f105-48f7-9bf0-38bdd351a4b9.png)


## Heres two possibilities to acess tvarv's values on your code:

If the class is avaiable on your ambient, you can use the abap 7.5 sintax and create the variable of parameter and request the information in one line!
If the class zcl_tvarv doesnt exist you can create it, full code by the end of the doc. 

    data(v_name) = zcl_tvarv=>get_single_value( 'Z_YOUR_TVARV' ).

Or
    
    zcl_tvarv=>get_single_value( EXPORTING input = 'Z_YOUR_TVARV' RECEIVING r_value = v_cenourinha ).

If you do not have this class in your ambient, you can use the select on the table that stands for both Parameter and Select Options. Here's an example:

```ABAP
"Define a range for directory names
DATA: lr_dir_range TYPE RANGE OF char50,
ls_dir_name LIKE LINE OF lr_dir_range,
vpath_tvarv TYPE CHAR255,

START-OF-SELECTION.

"Get the Directory Range from TVARVC

SELECT sign
opti
low
high
INTO TABLE lr_dir_range
FROM tvarvc
WHERE name = 'Z_MOBA' "The variable name given in STVARV
AND type = 'S'. "Select Option

READ TABLE lr_dir_range INTO DATA(w_dir) INDEX 1.

vpath_tvarv = w_dir-low.
```

Or

```abap

zcl_tvarv=>get_values( EXPORTING input = 'ZMM_COND_FRETE' IMPORTING output = DATA(lt_fretecode) ).

```

## Here's a Complete Example Using 7.5 Syntax
In this case, it was necessary to change the title of a report based on the material number (`matnr`), but only for specific matnr values that could vary between tenants.
The solution was to create a TVARV entry for each tenant, containing the matnr values that require a specific title.
> **Note:**  this is a snippet of a larger report.

```abap
CONSTANTS: c_specific_title  TYPE string VALUE 'SPECIFC TITLE FOR MATNR RETRIEVED FROM TVARV'.

* Select material descriptions
SELECT matnr spras maktx
  FROM makt
  INTO TABLE t_makt
  FOR ALL ENTRIES IN t_mara
  WHERE matnr EQ t_mara-matnr
    AND spras EQ sy-langu.
  
IF sy-subrc EQ 0.

  SORT t_makt BY matnr.

* Read TVARV entries and convert them into a range table
  SELECT * FROM tvarvc
    INTO TABLE @DATA(lt_tvarvc)
    WHERE name = 'Z_MAT_SPECIFIC'.

  lr_range_mat = VALUE ty_range_matnr(
    FOR tvarv IN lt_tvarvc
      ( sign   = tvarv-sign
        option = tvarv-opti
        low    = tvarv-low
        high   = tvarv-high )
  ).

* Inline loop: for records where matnr is in the range, override the title
* rule = for registers where matnr IN lr_range_mat, maktx = c_specific_title
  t_makt_aux = VALUE #( FOR row IN t_makt ( matnr = row-matnr
                                            spras = row-spras
                                            maktx = COND #( WHEN row-matnr IN lr_range_mat THEN c_specific_title
                                                            ELSE row-maktx ) ) ) .
  t_makt = t_makt_aux.
ENDIF.
```


## Complete code of ZCL_TVARV 

```abap
class ZCL_TVARV definition
  public
  final
  create public .

public section.

  class-methods GET_SINGLE_VALUE
    importing
      !I_PARAM type RVARI_VNAM
    returning
      value(R_VALUE) type RVARI_VAL_255 .
  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
  class-methods GET_TVARV
    importing
      value(INPUT) type ANY optional
      value(VALUE) type ANY optional
    returning
      value(CONTINUE) type CHAR1 .
  class-methods GET_VALUES
    importing
      value(INPUT) type ANY optional
      value(TYPE) type C optional
    exporting
      value(OUTPUT) type RSELOPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TVARV IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  endmethod.


  method CONSTRUCTOR.
  endmethod.


METHOD get_single_value.
  CHECK NOT i_param IS INITIAL.

  SELECT low UP TO 1 ROWS
    FROM tvarvc
    INTO r_value
   WHERE name EQ i_param
     AND type EQ 'P' ORDER BY PRIMARY KEY.
  ENDSELECT.

ENDMETHOD.


METHOD get_tvarv.


*   Types
*-------------------------------------*
  TYPES: BEGIN OF ty_tvarvc.
           INCLUDE TYPE tvarvc.
         TYPES: END OF ty_tvarvc.

*   Variables
*--------------------------------------*
  DATA: t_val  TYPE TABLE OF ty_tvarvc,
        wa_val LIKE LINE OF t_val.
  DATA: l_campo(50).

*   Field-Symbols
*--------------------------------------*
  FIELD-SYMBOLS:  <fs_val>   LIKE LINE OF t_val.
  FIELD-SYMBOLS: <fs_field> TYPE any,
                 <fs_vl>    TYPE any.
  FIELD-SYMBOLS: <fs> TYPE any.

*  Ranges
*--------------------------------------*
  DATA: r_range TYPE RANGE OF char10.

*   Data Types
  DATA: d_ref TYPE REF TO data.

*   Busca registros stvarv
  SELECT mandt name type numb sign opti low high clie_indep
    FROM tvarvc
    INTO TABLE t_val
    WHERE name EQ input.

  IF sy-subrc <> 0.
    MESSAGE i001(00) WITH TEXT-001 input.
    EXIT.
  ENDIF.

  "Cria range 'work area'
  CREATE DATA d_ref LIKE LINE OF r_range.
  ASSIGN d_ref->* TO <fs_vl>.

  "Cria range Dinamicamente
  LOOP AT t_val INTO wa_val.

    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <fs_vl> TO <fs_field>.
    IF sy-subrc = 0.
      <fs_field> = wa_val-sign.
    ENDIF.

    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <fs_vl> TO <fs_field>.
    IF sy-subrc = 0.
      <fs_field> = wa_val-opti.
    ENDIF.

    ASSIGN COMPONENT 'LOW' OF STRUCTURE <fs_vl> TO <fs_field>.
    IF sy-subrc = 0.
      <fs_field> = wa_val-low.
    ENDIF.

    ASSIGN COMPONENT 'HIGH' OF STRUCTURE <fs_vl> TO <fs_field>.
    IF sy-subrc = 0.
      <fs_field> = wa_val-high.
    ENDIF.

    APPEND <fs_vl> TO r_range.

  ENDLOOP.

  IF value IN r_range.
    continue = abap_true.
  ENDIF.

ENDMETHOD.


METHOD get_values.

  DATA: progname TYPE tvarv-name,
        l_campo  TYPE tvarvc-low.

  DATA: excep TYPE REF TO cx_root.
  DATA: v_str TYPE string.

*   Types
*-------------------------------------*
  TYPES: BEGIN OF ty_tvarvc.
          INCLUDE TYPE tvarvc.
  TYPES: END OF ty_tvarvc.

*   Variables
*--------------------------------------*
  DATA: t_val       TYPE TABLE OF ty_tvarvc,
        wa_val      LIKE LINE OF t_val.

*   Field-Symbols
*--------------------------------------*
  FIELD-SYMBOLS:  <fs_val>   LIKE LINE OF t_val.
  FIELD-SYMBOLS:  <fs_field> TYPE any,
                  <fs_vl>    TYPE any.
  FIELD-SYMBOLS: <fs> TYPE any.

*  Ranges
*--------------------------------------*
  DATA: r_range TYPE RANGE OF char10.

*   Data Types
*--------------------------------------*
  DATA: d_ref TYPE REF TO data.

  progname = input.

*   Grab lines from TVARV.
  SELECT mandt name type numb sign opti low high clie_indep
    FROM tvarvc
    INTO TABLE t_val
    WHERE name EQ progname.

  IF sy-subrc = 0.

    IF type IS SUPPLIED.
      DELETE t_val WHERE type NE type.
    ENDIF.

*   Create range work area, based on the type from the calling-program
    CREATE DATA d_ref LIKE LINE OF output. "r_range.
    ASSIGN d_ref->* TO <fs_vl>.

*   Mount range dinamically
    LOOP AT t_val INTO wa_val." FROM sy-tabix.

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <fs_vl> TO <fs_field>.
      IF sy-subrc = 0.

        IF wa_val-type EQ 'S' AND
           wa_val-sign IS INITIAL.
          <fs_field> = 'I'.
        ELSE.

          <fs_field> = wa_val-sign.

        ENDIF.

      ENDIF.

      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <fs_vl> TO <fs_field>.
      IF sy-subrc = 0.

        IF wa_val-type EQ 'S' AND
           wa_val-opti IS INITIAL.
          <fs_field> = 'EQ'.
        ELSE.

          <fs_field> = wa_val-opti.

        ENDIF.

      ENDIF.

      ASSIGN COMPONENT 'LOW' OF STRUCTURE <fs_vl> TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_val-low.
      ENDIF.

      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <fs_vl> TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_val-high.
      ENDIF.

      APPEND <fs_vl> TO output.

    ENDLOOP.

  ENDIF.

ENDMETHOD.
ENDCLASS.
```
