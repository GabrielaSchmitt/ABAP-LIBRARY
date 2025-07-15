# CDS Views

Este documento reúne conceitos, explicações e exemplos práticos sobre o uso de **CDS Views (Core Data Services)** em projetos SAP, com foco em **arquitetura limpa**, **performance** e **integrações com Fiori e OData**.
<br></br>
As **CDS Views (Core Data Services)** são uma forma avançada de modelar dados diretamente no banco SAP HANA. Elas permitem que parte da lógica de negócio seja empurrada para o banco de dados (*Code Push Down*), oferecendo:

- **Performance otimizada**
- **Reutilização de lógica de negócio**
- **Exposição nativa de dados via OData**
- **Integração com a camada de apresentação (Fiori)**

<br></br>
A criação e edição de CDS Views é feita no **ABAP Development Tools (ADT)** no **Eclipse**.  
Após ativar a CDS no Eclipse, é possível:
 - Visualizá-la na **SE11** como *View*.
 - Executá-la na **SE16N** como uma tabela normal.

**Atalhos úteis no Eclipse:**
 - `F8`: executa a CDS e mostra o resultado da seleção.
 - `Ctrl + Shift + F1`: aplica o Pretty Printer (identação automática).

<br></br>
## 🎯 Benefícios do uso de CDS

- ♻️ Reutilização de lógica de negócio
- ⚡ Alto desempenho (otimizado para SAP HANA)
- 🔐 Segurança integrada via DCL (Data Control Language)
- 🌐 Integração facilitada com SAP Fiori por meio de OData
- 📊 Suporte a relatórios analíticos com anotações específicas (`@Analytics`)
- 🧩 Arquitetura modular com camadas bem definidas (Basic → Composite → Consumption)

<br></br>

## 🧱 Arquitetura em camadas de CDS

As CDS Views seguem uma arquitetura em camadas, que melhora a organização e reutilização da lógica de negócio. As três camadas principais são:

- **Basic View**: acessa diretamente as tabelas SAP, trazendo dados brutos já normalizados.
- **Composite View**: utiliza uma ou mais Basic Views e aplica a lógica de negócio (joins, filtros, cálculos).
- **Consumption View**: consome as Composite Views e adiciona anotações (`@UI`, `@OData.publish`, etc.) para entrega final em relatórios, APIs ou apps Fiori.

```plaintext
Tabelas SAP
   |
   ▼
┌────────────┐     ┌────────────┐     ┌────────────────────┐     ┌────────────────────────────┐
│ Basic View │ --> │ Composite  │ --> │ Consumption View   │ --> │ Fiori / Relatórios / OData │
│ (dados     │     │ View       │     │ (apresentação, UI) │     │ (consumo final)            │
│ brutos)    │     │ (lógica)   │     │                    │     │                            │
└────────────┘     └────────────┘     └────────────────────┘     └────────────────────────────┘
```

## Como buscar CDS existentes:

Utilize o botão mostrado na imagem abaixo no Eclipse: 
<img width="823" height="180" alt="image" src="https://github.com/user-attachments/assets/76f1e6e7-449c-4c6a-a9f0-d4c31fccaaf9" />

Para fazer busca e filtragem de nome utilize a tag (`type`) para definir o tipo de objeto que deseja buscar e depois busque pelo nome, a utilização de (`*`) funciona da mesma forma que no sap, como no exemplo abaixo. 
<img width="502" height="307" alt="image" src="https://github.com/user-attachments/assets/e8cf5ad6-3556-45e1-b8f5-b0853e540776" />

<br></br>

## JOIN vs ASSOCIATION:

Ao realizar relacionamentos de dados em uma CDS Composite View, existem duas abordagens principais:

**JOIN**: indicado quando os dados relacionados precisam ser carregados imediatamente na execução da view.
Exemplo típico: trazer o nome do cliente junto ao pedido na mesma estrutura de resultado.

**ASSOCIATION**: recomendado para manter a modelagem relacional entre entidades sem carregar os dados diretamente, a menos que sejam explicitamente acessados (ideal para apps Fiori e APIs OData).

> 🔍 Abaixo, um exemplo comparativo entre as duas abordagens para a mesma lógica de relacionamento entre VBAK e KNA1:

<table>
  <tr>
    <th>JOIN</th>
    <th>ASSOCIATION</th>
  </tr>
  <tr>
    <td>
      <pre><code class="language-abap">
define view ZCDS_SalesOrderJoin 
  as select from vbak
    inner join kna1 
      on vbak.kunnr = kna1.kunnr
{
  vbak.vbeln,
  kna1.name1
}
      </code></pre>
    </td>
    <td>
      <pre><code class="language-abap">
define view ZCDS_SalesOrderAssoc 
  as select from vbak
    association [0..1] to kna1 as _Customer 
      on vbak.kunnr = _Customer.kunnr
{
  vbak.vbeln,
  _Customer.name1
}
      </code></pre>
    </td>
  </tr>
</table>

<br></br>
## Session Variables
As session variables permitem acessar dados da sessão atual (usuário, idioma, client, data etc.) diretamente na CDS View, adaptando o resultado conforme o contexto da execução, sem precisar de parâmetros manuais.

| Session Variable CDS        | Exemplo                   | Equivalente ABAP | Descrição                          |
|----------------------------|---------------------------|------------------|------------------------------------|
| `$session.client`          | `$session.client`         | `sy-mandt`       | Client atual                       |
| `$session.system_date`     | `$session.system_date`    | `sy-datum`       | Data do servidor                   |
| `$session.system_language` | `$session.system_language`| `sy-langu`       | Idioma do logon                    |
| `$session.user`            | `$session.user`           | `sy-uname`       | Usuário atual                      |
| `$session.user_date`       | `$session.user_date`      | `sy-datlo`       | Data local do usuário              |
| `$session.user_timezone`   | `$session.user_timezone`  | `sy-zonlo`       | Fuso horário do usuário            |


```abap
define view ZCDS_SessionExample
  as select from t001
{
  bukrs,
  butxt,
  $session.client          as ClientField,
  $session.system_date     as Today,
  $session.system_language as SystemLanguage,
  $session.user            as CurrentUser
}
where mandt = $session.client
```
> Em CDS Views, o filtro pelo mandante (client) é aplicado automaticamente com base no sy-mandt da sessão do usuário. Isso garante que os dados acessados pertençam apenas ao client atual, sem necessidade de declarar ou filtrar o campo MANDT.

<br></br>

## CAST 

Operações `cast` permitem forçar ou transformar o tipo de um valor ou campo em tempo de definição da CDS View. São essenciais para garantir tipos corretos em cálculos e joins, aplicar precisão em campos decimais e preparar dados para consumo via OData ou Fiori.

### Principais tipos de cast 

| Tipo ABAP CDS      | Exemplo                                                  | Descrição                          |
|--------------------|----------------------------------------------------------|------------------------------------|
| `abap.char(N)`     | `cast('ABC' as abap.char(10))`                            | Texto de comprimento fixo         |
| `abap.numc(N)`     | `cast('123' as abap.numc(5))`                             | Numérico em formato texto         |
| `abap.dats`        | `cast('20240714' as abap.dats)`                           | Data (formato AAAAMMDD)           |
| `abap.tims`        | `cast('123000' as abap.tims)`                             | Hora (formato HHMMSS)             |
| `abap.int4`        | `cast(10 as abap.int4)`                                   | Número inteiro (4 bytes)          |
| `abap.fltp`        | `cast(1.25 as abap.fltp)`                                 | Ponto flutuante                    |
| `abap.dec(P,S)`    | `fltp_to_dec(1.25 as abap.dec(5,2))`                      | Decimal com precisão (P,S)        |
| `abap.lang`        | `cast('E' as abap.lang)`                                  | Código de idioma                   |
| `abap.unit`        | `cast('KG' as abap.unit)`                                 | Unidade de medida (ex: KG, M)     |
| `abap.curr`        | `cast('BRL' as abap.curr)`                                | Moeda (ex: BRL, USD)              |


```abap
//Cast aninhado -> o uso de preserving type garante que o sistema respeite o tipo do elemento de dados final 
cast(  cast('E' as abap.lang) as syclang preserving type) as LanguageField
```
<br></br>

## Typed Literals 
Typed literals são valores constantes com tipo ABAP explícito definidos diretamente na CDS. Evitam erros de conversão, comparações incorretas ou perda de precisão, e são especialmente úteis em filtros, casts e cálculos.

```abap
// Caracter
'ABC'                              as RawChar,
abap.char 'ABC'                    as TypedChar,
cast('ABC' as abap.char(10))       as CastChar,

// Decimal e ponto flutuante
123.45                             as RawFloat,
abap.fltp '123.45'                 as TypedFloat,
fltp_to_dec(123.45 as abap.dec(6,2)) as ConvertedDecimal,

// Com preserving type (mantém metadados do tipo alvo)
cast( abap.char 'XYZ' as Char10 preserving type ) as PreciseTypedChar
```
<br></br>

## Simple Types 
Tipos simples permitem criar alias reutilizáveis baseados em tipos primitivos ABAP (como abap.lang, spras, etc.), melhorando a clareza, reutilização e padronização de dados em CDS Views.

```abap
define type ZBT_LanguageA : abap.lang;     // baseado em tipo ABAP
define type ZBT_LanguageB : spras;         // baseado em elemento de dados
define type ZBT_LanguageD : ZBT_LanguageC; // hierarquia de tipos simples
```

Exemplo de uso em uma cds
```abap
define view entity Z_ViewWithSimpleTypes
  as select distinct from t000
{
  cast( abap.lang'E' as ZBT_LanguageA preserving type ) as Language1,
  cast( abap.lang'E' as ZBT_LanguageD preserving type ) as Language2
}
```
<br></br>

## CASE
Permite criar lógicas condicionais diretamente em CDS Views, facilitando o tratamento de variações nos dados sem precisar codificar regras adicionais em ABAP.
Ela retorna valores diferentes com base em condições, funcionando como um switch.

```abap
// exemplo 1
case product_type
  when 1 then 'X'
  when 2 then 'Y'
  else ''
end as Case1,

// exemplo 2
case
  when product_type = 1 then 'X'
  when product_type = 2 then 'Y'
  when product_type = 3 or product = 4 then 'X'
  else ''
end as Case2

//exemplo 3 (com cast)
cast(
  case (SalesOrderType)
    when 'TAF' then 'X'
    when 'OAF' then 'X'
    else ''
  end as abap.char(3)
) as IsStandardOrderAsChar3

```
<br></br>

## Union Views
As Union Views permitem combinar dados de diferentes fontes em uma única CDS View. Isso é útil para consolidar registros semelhantes vindos de tabelas diferentes ou views com estruturas compatíveis.

**Regras**: 
- Todas as branches (selects) devem ter os mesmos campos e na mesma ordem.
- Os nomes e tipos dos campos devem ser compatíveis.
- O tipo efetivo de cada campo é determinado pelo primeiro select da união.
- Anotações (@EndUserText, por exemplo) só são consideradas da primeira branch.
- Use @Metadata.ignorePropagatedAnnotations: true para evitar propagação de anotações indesejadas.

### Lógica de União (UNION)
Ao aplicar a lógica de união, registros duplicados originários de diferentes fontes de dados mescladas são automaticamente removidos da lista de resultados.

**Exemplo**: Aplicando a lógica de união sobre a mesma fonte de dados `Z_ViewAsDataSourceA`, a implementação da CDS View `Z_UnionViewWithoutDuplicates` resulta em um único registro de dados.

```abap
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_UnionViewWithoutDuplicates
  as select from Z_ViewAsDataSourceA
{
  key FieldA1
}
union select from Z_ViewAsDataSourceA
{
  key FieldA1
}
```

**Resultado**: A remoção de duplicatas simplifica o conjunto final de dados, garantindo que cada registro seja único.

### Lógica de Union All (UNION ALL)
Se a remoção automática de duplicatas não for desejada, utilize a lógica union all, que mantém todos os registros das fontes de dados envolvidas.

**Comparação de Resultados**: Ao contrário da view anterior, que elimina duplicatas, a aplicação da lógica union all na CDS View `Z_UnionViewWithDuplicates` resulta em dois registros de dados idênticos.

```abap
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_UnionViewWithDuplicates
  as select from Z_ViewAsDataSourceA
{
  FieldA1
}
union all select from Z_ViewAsDataSourceA
{
  FieldA1
}
```

**Escolha da Lógica**: A decisão entre `union` e `union all` depende da necessidade de manter ou remover registros duplicados no resultado final.

```abap
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_UnionView
  as select from Z_ViewAsDataSourceA
{
  key FieldA1 as UnionField1,
  key FieldA2 as UnionField2,
  key FieldA3 as UnionField3
}
union select from Z_ViewAsDataSourceB
{
  key FieldB2 as UnionField1,
  key FieldB1 as UnionField2,
  key ''      as UnionField3
}
```
<br></br>

## INTERSECT e EXCEPT

As operações `INTERSECT` e `EXCEPT` são utilizadas para determinar registros comuns ou exclusivos entre diferentes fontes de dados em CDS Views. Essas operações são fundamentais para análises comparativas e identificação de diferenças entre conjuntos de dados.

O `INTERSECT` identifica registros que estão presentes em **ambas** as fontes de dados simultaneamente, enquanto o `EXCEPT` exclui registros de uma fonte que estão presentes em outra, retornando apenas os registros exclusivos da primeira fonte.

<table>
  <tr>
    <th>INTERSECT</th>
    <th>EXCEPT</th>
  </tr>
  <tr>
    <td>
      <pre><code class="language-abap">
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_ViewWithIntersect
  as select from Z_UnionViewAsDataSourceA
{
  key Field1
}
intersect select from Z_UnionViewAsDataSourceB
{
  key Field1
}
      </code></pre>
    </td>
    <td>
      <pre><code class="language-abap">
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_ViewWithExcept
  as select from Z_UnionViewAsDataSourceA
{
  key Field1
}
except select from Z_UnionViewAsDataSourceB
{
  key Field1
}
      </code></pre>
    </td>
  </tr>
</table>

| Operação | Descrição | Resultado |
|----------|-----------|-----------|
| **UNION** | Combina registros de ambas as fontes | Todos os registros únicos |
| **UNION ALL** | Combina registros mantendo duplicatas | Todos os registros, incluindo duplicatas |
| **INTERSECT** | Registros presentes em ambas as fontes | Apenas registros comuns |
| **EXCEPT** | Registros da primeira fonte que não estão na segunda | Registros exclusivos da primeira fonte |

### Exemplo comparativo

Considerando duas fontes de dados:
- **Z_UnionViewAsDataSourceA**: contém registros [A, B]
- **Z_UnionViewAsDataSourceB**: contém registros [A, C]

| Operação | Resultado |
|----------|-----------|
| **UNION** | [A, B, C] |
| **INTERSECT** | [A] |
| **EXCEPT** (A - B) | [B] |
| **EXCEPT** (B - A) | [C] |

**Regras importantes**:
- Todas as queries devem ter a mesma estrutura de campos
- Os tipos de dados devem ser compatíveis
- A ordem dos campos deve ser idêntica
- Use `@Metadata.ignorePropagatedAnnotations: true` para controlar propagação de anotações
<br></br>

