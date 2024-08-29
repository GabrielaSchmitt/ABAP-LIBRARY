### The idea of this code is to introduce a delay in the program without using the built-in syntax WAIT UP TO.
### In this example, suppose you have a class, and you can create the method shown below.

```abap

  METHOD wait_millisecs.
    DATA lv_timeend TYPE tzntstmpl.
    DATA lv_timebeg TYPE tzntstmpl.
    DATA lv_milisec TYPE tzntstmpl.

    lv_milisec = iv_millisecs / 1000.

    GET TIME STAMP FIELD lv_timebeg.
    ADD lv_milisec TO lv_timebeg.

    DO.
      GET TIME STAMP FIELD lv_timeend.
      IF lv_timebeg < lv_timeend.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

```

### To use it, you can call it like this:

```abap

zcl_your_class=>wait_millisecs( 1000 )
```
