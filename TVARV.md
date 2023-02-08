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

After creating you simply has to click on save and its done. ![image](https://user-images.githubusercontent.com/86369677/217554249-1c050602-f105-48f7-9bf0-38bdd351a4b9.png)


## Heres two possibilities to acess tvarv's values on your code:

If the class is avaiable on your ambient, you can use the abap 7.5 sintax and create the variable of parameter and request the information in one line!

    data(v_name) = zcl_tvarv=>get_single_value( 'Z_YOUR_TVARV' ).
    
    zcl_tvarv=>get_single_value( EXPORTING input = 'Z_YOUR_TVARV' RECEIVING r_value = v_cenourinha ).

If you do not have this class in your ambient, you can use the select on the table that stands for both Parameter and Select Options. Here's an example:

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
