# CDS Timestamp to Date Conversion

## Overview
**ZCDS_VALID_TO_DATE** is a custom CDS View used to convert `timestamp` fields (`ValidFrom`, `ValidTo`) from the standard view **I_BusinessPartner_to_BP_Role** into SAP `date` format (`YYYYMMDD`). This conversion allows direct date comparisons, such as checking whether a Business Partner role is still valid on the current system date.

## View Definition
```abap
@AbapCatalog.sqlViewName: 'ZVALIDTOVW'
@EndUserText.label: 'Convert valid_to timestamp to date'
define view ZCDS_VALID_TO_DATE
  as select from I_BusinessPartner_to_BP_Role
{
  key BusinessPartner,
      BusinessPartnerRole,
      ValidFrom,
      tstmp_to_dats(
        ValidFrom,
        abap_system_timezone( $session.client,'NULL' ),
        $session.client,
        'FAIL'
      ) as datefrom,
      ValidTo,
      tstmp_to_dats(
        ValidTo,
        abap_system_timezone( $session.client,'NULL' ),
        $session.client,
        'FAIL'
      ) as dateto
}
```

## Examle usage in a MAIN cds 

```abap
@AbapCatalog.sqlViewName: 'ZMAIN'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MAIN'
@OData.publish: true
define view Z_MAIN_EXAMPLE
as select from    I_CreditManagementAccount      as ac
//Consulta Categoria informacao do BP
    left outer join ZCDS_VALID_TO_DATE           as bi   on  bi.BusinessPartner     = ac.BusinessPartner
                                                         and bi.dateto              >= $session.system_date
{
  key ac.BusinessPartner,
      bi.datefrom                               as DataFrom,
      bi.dateto                                 as DataTo
}

```


## Expected Results
When accessing the view through transaction SE16N with fictional data, you should see the conversion results as shown below:

<img width="1123" height="438" alt="image" src="https://github.com/user-attachments/assets/4131dd76-d7b3-4fd6-90ec-d6420e23cc12" />

<img width="813" height="331" alt="image" src="https://github.com/user-attachments/assets/4b067d28-270c-4774-b212-b458908bafd9" />
