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

<br></br>

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
