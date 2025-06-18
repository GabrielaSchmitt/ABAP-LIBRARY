// Every line referring to 'STATUS' has to be changed to your domain scenario, please refer to the code above as an example.
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CIB: VALUE HELP STATUS'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
@ObjectModel: {
                dataCategory: #VALUE_HELP,
                representativeKey: 'NrPesagem',
                usageType.sizeCategory: #L,
                usageType.dataClass: #TRANSACTIONAL,
                usageType.serviceQuality: #C,
                supportedCapabilities: [#VALUE_HELP_PROVIDER, #SEARCHABLE_ENTITY],
                modelingPattern: #VALUE_HELP_PROVIDER
                 }
@Search.searchable: true
@Consumption.ranked: true
define view entity ZMM_CIB_VH_STATUS as select distinct from dd07t
{
    @ObjectModel.filter.enabled: false
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Search.ranking: #HIGH
    @EndUserText:{ label: 'Código' , quickInfo: 'Código' }  
    @ObjectModel.text.element: ['StatusDesc']
    key domvalue_l as Status,
    
    @ObjectModel.filter.enabled: false
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Search.ranking: #HIGH
    @EndUserText:{ label: 'Status' , quickInfo: 'Status' }    
    ddtext as StatusDesc   
    
    
} where domname = '/SPROAGBL/D_STATUS' and ddlanguage = $session.system_language
