## Criticality (Semáforo) em Aplicações RAP Fiori 🚦

O Criticality é um recurso de UI que melhora a experiência do usuário ao exibir um campo de status com ícones e cores padronizadas (vermelho, amarelo, verde), fornecendo um feedback visual imediato e intuitivo sobre o estado de um registro.

Para implementa-lo é necessário um entrelaçamento entre a camada de dados (CDS View) com a camada de apresentação (Metadata Extension):

**Lógica de Criticidade na CDS View**: Um campo numérico é criado para mapear os diferentes status do negócio para os valores de criticidade reconhecidos pelo Fiori.

**Associar a Criticidade na Camada de Metadados**: Uma anotação de UI é usada para vincular o campo de status visual (o que o usuário vê) ao campo de lógica de criticidade. 

---
<br></br>

### Exemplo 1: CDS e Metadata

<img width="1245" height="603" alt="image" src="https://github.com/user-attachments/assets/f9ea1635-4f1d-4d7a-b96a-84ae1534a756" />

> Na sua CDS View, adicione um novo campo usando uma expressão `CASE`. Este campo irá traduzir o status do texto para um valor numérico.

<br></br>

**CDS**
```abap
define view entity ZI_TRAVEL_U01158 
  as select from ztravel_u01158 as Travel
{
  //... outros campos da view
  
  case Travel.overall_status
    when 'A' then 3 -- Accepted (Verde)
    when 'O' then 2 -- Open (Amarelo)
    when 'X' then 1 -- Rejected (Vermelho)
    else 0         -- Neutro
  end as OverallStatusCriticality,
  
  // Campo de status original que será exibido na tela
  Travel.overall_status as OverallStatus
  
  //... outras associações e campos
}
```

**Metadata**
```abap
@Metadata.layer: #CORE
@UI: {
  headerInfo: { typeName: 'Travel', typeNamePlural: 'Travels' }
}
annotate view ZC_TRAVEL_U01158
  with 
{
  @UI.selectionField: [{ position: 10 }]  
  @UI.lineItem:  [ { position: 10, criticality: 'OverallStatusCriticality' } ]
  @UI.DataPoint: [ { qualifier: 'StatusData', title: 'Status', criticality: 'OverallStatusCriticality' } ]
  OverallStatus;
  
  //...
}
```

---
<br></br>

### Exemplo 2: Domínio, Search Help, CDS e Metadata
<br></br>
<img width="1834" height="773" alt="image" src="https://github.com/user-attachments/assets/cda11fd8-e67b-4af6-b819-efa83e800c94" />
<br></br>
<img width="928" height="608" alt="image" src="https://github.com/user-attachments/assets/517fe87d-9651-49b8-bf67-39defbc4e750" />
<br></br>

**Domínio**

<img width="936" height="468" alt="image" src="https://github.com/user-attachments/assets/94bb5cde-4b8d-48c0-a1fc-554a411a56a4" />

**CDS Value Help**
```abap
    @AbapCatalog.sqlViewName: 'ZSDVSTATPVH'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - Status de Pagamento'
define view ZSD_I_STATUS_PAGTO_VH
  as select from dd07t
{
  key cast( domvalue_l as numc1 ) as StatusCode, 
      ddtext                     as StatusText  // 'Pago' ou 'Pendente'
}
where
      domname    = 'ZSD_DO_STATUS_PAGTO'      
  and ddlanguage = $session.system_language   
```

**CDS**
```abap
      case
        when
         cast( case when count( distinct _bseg.buzei ) > 0 then
          \\ calculo que retorna 0 quando quitado(pago) e > 0 quando há saldo devedor(pendente)
               ( ( ( ceil( ( ( cast( _vbrp.netwr as abap.dec(15, 2) ) / 100 ) * _salesorderitem.ConditionRateValue ) * 100 ) / 100 ) / count( distinct _bseg.buzei ) ) * ( count( distinct _bseg.buzei ) - count( distinct case when _bseg.augdt is not initial and _bseg.augdt <> '00000000' then _bseg.buzei else null end ) ) )
          else 0 end as abap.dec(15, 2) )
         > 0
        then 2  // 2 = Amarelo (Pendente)
        else 3  // 3 = Verde (Pago)
      end as StatusPagamento
```

**Metadata**
```abap
  @UI.lineItem: [ {
  position: 5,                       
  importance: #HIGH,
  label: 'Status',
  criticality: 'StatusPagamento',    
  criticalityRepresentation: #WITH_ICON 
   } ]
  @UI.selectionField: [{ position: 1 }]  
  @EndUserText.label: 'Status do Pagamento'
  @Consumption.filter.defaultValue: '2'  // <-- FILTRO PARA INICIAR O APP COM OS DADOS 'PENDENTE'
  @Consumption.valueHelpDefinition: [{
    entity: {
      name: 'ZSD_I_STATUS_PAGTO_VH', 
      element: 'StatusCode'
    }
  }]
  StatusPagamento;
```
