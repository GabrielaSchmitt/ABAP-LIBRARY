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
// exemplos
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
