# ABAP RESTful Application Programming Model (RAP)

Este documento reúne conceitos, explicações e exemplos práticos sobre o uso do **ABAP RESTful Application Programming Model (RAP)**, a arquitetura padrão da SAP para o desenvolvimento de aplicações Fiori e Web APIs otimizadas para SAP HANA, tanto no ambiente On-Premise quanto na nuvem (BTP).

O **ABAP RAP** representa a evolução do desenvolvimento ABAP, fornecendo um modelo de programação que padroniza a criação de serviços OData de ponta a ponta. Com o S/4HANA, modificações tecnológicas fundamentais foram introduzidas no ABAP, impactando todas as camadas do desenvolvimento, desde o banco de dados SAP HANA até a experiência do usuário com SAP Fiori e SAPUI5.

> **Desenvolvimento Eficiente**: Reduz a complexidade ao fornecer frameworks que automatizam tarefas técnicas.

> **Consistência e Padronização**: Garante que as aplicações sigam uma arquitetura consistente e um fluxo de desenvolvimento padronizado.

> **Otimizado para SAP HANA:** O RAP aproveita ao máximo a velocidade do banco de dados SAP HANA por meio do `"Code Pushdown"` (isto é, em vez de trazer todos os dados brutos para a aplicação, a lógica e os cálculos são 'empurrados' para serem executados diretamente lá no banco de dados).
Isso acaba com a necessidade de mover grandes volumes de dados para a memória do servidor de aplicação (uma prática antiga que era lenta e consumia muitos recursos de rede e processamento).
Essa performance toda é possível porque a tecnologia `in-memory` do HANA (que guarda os dados na memória RAM, milhares de vezes mais rápida que discos tradicionais) armazena os dados na memória principal do banco e utiliza um `armazenamento em colunas` (que organiza os dados de forma vertical, acelerando cálculos) com `indexação automática` para acelerar o acesso (pense nisso como um índice de livro super inteligente e sempre atualizado para cada coluna de dados, permitindo ao banco de dados encontrar qualquer informação instantaneamente, sem precisar "ler" a coluna inteira).

> **Baseado em REST**: As APIs geradas seguem os princípios REST, garantindo interoperabilidade e escalabilidade.

O desenvolvimento com RAP é realizado no **ABAP Development Tools (ADT)** no Eclipse.

<br>

## 🎯 Benefícios do uso de RAP

- ♻️ **Reutilização de Lógica**: A lógica de negócio é implementada de forma agnóstica ao protocolo, permitindo sua reutilização.
- ⚡ **Alto Desempenho**: Otimizado para SAP HANA, garantindo a melhor performance possível para as aplicações.
- ☁️ **Cloud-Ready**: É o modelo de programação padrão para o desenvolvimento na nuvem com o SAP BTP ABAP Environment.
- 🧩 **Arquitetura Orientada a Serviços**: Facilita a exposição de dados e funcionalidades como serviços OData.
- 🔒 **Segurança Integrada**: Oferece mecanismos para controle de acesso e autorizações.
- 🌐 **Foco na Lógica de Negócio**: Abstrai a complexidade técnica, permitindo que o desenvolvedor se concentre no que realmente importa.

<br>

## 🧱 Arquitetura e Fluxo de Desenvolvimento

O desenvolvimento de uma aplicação RAP segue um fluxo bem definido, partindo da base de dados até a exposição do serviço.

1.  **Tabelas de Banco de Dados (Database Tables)**: A base de tudo. Podem ser tabelas existentes (padrão ou Z) ou novas tabelas criadas especificamente para a aplicação.
2.  **Modelo de Dados (Data Model)**: Definido com **Core Data Services (CDS)**. As CDS Views, como a *View Entity* (raiz) e a *Projection View*, criam um modelo semântico sobre as tabelas, permitindo modelar os dados diretamente no banco.
3.  **Comportamento (Behavior)**: O comportamento transacional (CRUD) é definido na **Behavior Definition (BDL)**  e implementado em uma classe ABAP chamada **Behavior Pool**.
4.  **Projeção (Projection)**: Uma camada de projeção, composta por *Projection Views* e *Behavior Projections*, é criada para expor uma visão específica do modelo de dados e do comportamento para o consumidor, permitindo limitar campos e adaptar a lógica.
5.  **Definição do Serviço (Service Definition)**: Define quais entidades do modelo de dados serão expostas no serviço.
6.  **Vinculação do Serviço (Service Binding)**: Especifica o protocolo de comunicação (OData V2 ou OData V4) e o tipo de consumidor (UI ou Web API). É aqui que o serviço se torna testável.

```plaintext
┌──────────────────┐     ┌──────────────────┐      ┌───────────────┐      ┌─────────────┐     ┌──────────────────┐     ┌────────────────┐
│      DB Table    │ --> │    Data Model    │ -->  │   Behavior    │ -->  │  Projection │ --> │ Service          │ --> │ Service        │
│                  │     │      (CDS)       │      │ (BDL + Class) │      │ (CDS + BDL) │     │ Definition       │     │ Binding (OData)│
└──────────────────┘     └──────────────────┘      └───────────────┘      └─────────────┘     └──────────────────┘     └────────────────┘
```

<br>

## OData: O Protocolo de Comunicação

O **OData (Open Data Protocol)** é o padrão usado pelo RAP para expor serviços como **APIs REST**. Ele define como os dados são trocados entre cliente e back-end.  

Uma URL OData é formada por:  
- **Service Root URL** → endereço base do serviço.  
- **Resource Path** → entidade específica acessada.  
- **Query Options** → filtros e ordenações (ex: `$top`, `$orderby`).  

### 📌 Anotações e Vocabulários
As **Annotations** e **Vocabularies** adicionam semântica ao modelo de dados, ajudando a UI (ex: definir que o título de "Pessoa" seja `LastName + FirstName`).  

### 🔹 CRUD via HTTP

No RAP, as operações de manipulação de dados seguem o padrão REST, mapeando diretamente os métodos HTTP para as ações CRUD:  

| Método | CRUD | Descrição |
| :--- | :--- | :--- |
| GET | Read | Leitura de dados |
| POST | Create | Criação de dados |
| PUT | Update (total) | Atualização completa de uma entidade |
| PATCH | Update (parcial) | Atualização parcial de atributos |
| DELETE | Delete | Exclusão de dados |

O **OData V4**, padronizado pela OASIS e ISO/IEC, é a versão recomendada pela SAP e traz diversas vantagens sobre o V2 como `Melhor compactação de metadados` economizando volume de dados, `Consultas mais sofisticadas` reduzindo o número de chamadas, `Recursos analíticos avançados` garantindo acesso a múltiplos serviços simultaneamente pelo client.

### OData V2 vs OData V4

| Feature      | OData V2                        | OData V4                                |
|--------------|---------------------------------|------------------------------------------|
| Adoption     | Gateway, Legacy apps            | RAP, Modern apps                         |
| Metadata     | XML only                        | XML + JSON                               |
| Operations   | CRUD + Function Imports         | CRUD + Bound/Unbound Actions             |
| Annotations  | Limited, SAP-specific           | Rich, standard-based                     |
| Payload      | XML/JSON (heavy)                | JSON-first (lightweight, faster)         |
| Semantics    | Associations, Navigations       | Compositions, stronger semantics         |
| Batch        | Basic                           | More efficient                           |

<br>

## SAP Fiori Elements: Acelere o Desenvolvimento de UI

**SAP Fiori Elements** fornece modelos (templates ou *floorplans*) para cenários de negócios comuns, permitindo criar aplicações SAPUI5/Fiori consistentes e prontas para o uso empresarial sem escrever código de UI. O layout e o comportamento da aplicação são determinados pelos metadados do serviço OData e pelas anotações nas CDS Views.

### Benefícios do Fiori Elements

  - **Eficiência**: O desenvolvedor foca na lógica de negócio no back-end, escrevendo menos código de UI.
  - **Consistência de UX**: Garante um design uniforme, com navegação, filtros e mensagens padronizadas, em conformidade com as diretrizes mais recentes do SAP Fiori.
  - **Prontidão Empresarial**: Oferece suporte nativo a acessibilidade, internacionalização, responsividade e segurança.
  - **Ferramentas de Suporte**: As **SAP Fiori tools**, disponíveis no VS Code e no SAP Business Application Studio, auxiliam na configuração das aplicações.

### Modelos (Floorplans) Comuns

  - **List Report Page**: O modelo mais comum, ideal para exibir, filtrar e navegar por grandes conjuntos de dados. Geralmente é o ponto de entrada para a página de objeto.
  - **Object Page**: Usada para exibir todos os detalhes de um único objeto de negócio. O conteúdo é organizado em seções para facilitar a visualização e edição.
  - **Overview Page**: Fornece uma visão geral de uma área de negócio através de "cards" interativos, que servem como ponto de entrada para outros processos.
  - **Analytical List Page**: Adiciona recursos analíticos, como gráficos e filtros visuais, a uma lista transacional para visualizar e analisar dados de diferentes perspectivas.
  - **Worklist**: Permite que os usuários processem uma lista de itens de trabalho que exigem ação.

## 📜 Evolução dos Modelos de Programação ABAP

Para entender completamente o RAP, é essencial conhecer a trajetória dos modelos de programação que o precederam. A imagem a seguir ilustra a sequência cronológica desde o ABAP Clássico até o moderno ABAP RAP.

| Plataforma | Modelo de Programação | Principais Componentes |
| :--- | :--- | :--- |
| **ABAP Platform < 7.50** | Freestyle ABAP Programming | Modelagem de dados, lógica de negócio e UIs com tecnologias clássicas. |
| **ABAP Platform >= 7.50** | ABAP Programming Model for SAP Fiori | SAP Gateway, OData, Annotations, CDS e BOPF (CDS-Based). |
| **SAP S/4HANA >= 1909** | Application Programming Model (RAP) | Business Service, Behavior Definition, Behavior Implementation, CDS. |

### ABAP Clássico (Freestyle)

Criado para o SAP R/3 e centrado no SAP Netweaver Application Server, o ABAP Clássico era caracterizado por:
- **Data Modeling**: Criação de tipos globais, estruturas e tabelas no dicionário ABAP.
- **Business Logic**: Implementada através de Forms, Funções e Classes.
- **UI Development**: As interfaces de usuário eram baseadas em Webdynpro e tecnologias de GUI como o ALV.
- **Interface Development**: A comunicação com sistemas externos era feita via IDOC, SOAP, HTTP, RFC e SMTP.

### ABAP Programming Model for SAP Fiori

Com a chegada do S/4HANA e a necessidade de UIs modernas, surgiu o "ABAP Programming Model for SAP Fiori". Este modelo foi a ponte para o RAP e introduziu tecnologias chave:

- **CDS (Core Data Services)**: Para a modelagem de dados que seriam expostos via OData.
- **BOPF (Business Object Processing Framework)**: O primeiro framework a apoiar a implementação direta da lógica de negócio de um aplicativo. Ele definia o fluxo de processamento e era estruturado em nós, atributos, validações, determinações e ações.
- **SAP Gateway**: Servindo como a ponte entre o mundo ABAP e o protocolo OData. Ele permite que os desenvolvedores realizem leituras e escritas sem precisar conhecer os detalhes do OData, expondo os serviços CDS de forma simplificada.
- **SAPUI5**: A tecnologia para o desenvolvimento das UIs modernas e baseadas em navegador que compõem os aplicativos Fiori.

A arquitetura deste modelo integrava o SAP HANA Database, o backend com CDS e BOPF, e o frontend com o SAP Gateway, que por sua vez se comunicava via OData/HTTPS com a camada de UI (SAPUI5 e Fiori Elements).

<br>

## 🔄 Modelo Transacional do RAP

O modelo transacional do RAP é fundamental para entender como as operações de modificação de dados são processadas. Ele se distingue por ocorrer em duas fases principais.

1.  **Fase de Interação (Interaction Phase)**: Quando um objeto de negócio é modificado (criado, atualizado ou deletado), as alterações não são escritas diretamente no banco de dados. Em vez disso, as instâncias das respectivas CDS são armazenadas em um buffer transacional na memória.
2.  **Fase de Salvamento (Save Sequence)**: Após a interação, um gatilho de "save" é acionado pelo consumidor (por exemplo, ao clicar no botão "Salvar" em um app Fiori). Neste momento, o estado do buffer é persistido e finalmente escrito na base de dados.

### Tratamento de Rascunho (Draft Handling) 
Uma das funcionalidades mais poderosas do RAP é o tratamento de rascunhos, que está totalmente implementado pelo framework.
- Ele permite que o estado do buffer transacional seja armazenado de forma temporária, mas persistente, no banco de dados.
- Isso possibilita que um usuário inicie uma transação, pare e continue seu trabalho posteriormente, mesmo em um dispositivo diferente, sem perder os dados inseridos.
- Essa capacidade de distribuir a fase de interação entre várias sessões ou solicitações é feita sem violar os princípios de comunicação sem estado do REST.
> ⚠️ é uma capacidade standard portanto só funciona quando é do Implementation Type `MANAGED`. 

<br>

## 🛠️ Tipos de Implementação: Managed vs. Unmanaged

O modelo RAP introduz o conceito de **Implementation Type** para especificar quem é responsável por fornecer a implementação da lógica de leitura (Query) e de escrita (Behavior). A escolha entre os dois tipos define como o comportamento da aplicação será desenvolvido.

| Tipo | Descrição | Cenário de Uso |
| :--- | :--- | :--- |
| **Managed (Gerenciado)** | Utiliza a implementação pronta do RAP. O framework fornece automaticamente o comportamento CRUD padrão para as CDSs e gerencia o buffer transacional. A funcionalidade fica disponível sem programação adicional, mas o desenvolvedor pode adicionar novas lógicas para complementar o comportamento padrão. | Ideal para cenários "Greenfield" (desenvolvimento de novas aplicações), onde não há lógica legada a ser reutilizada e se busca máxima aceleração no desenvolvimento. |
| **Unmanaged (Não Gerenciado)** | O desenvolvedor precisa implementar todas as funcionalidades, incluindo a fase de interação com o buffer. Este tipo permite integrar APIs e lógicas de negócio já existentes, envolvendo-as no modelo RAP. | Frequentemente utilizado em cenários "Brownfield", onde é necessário reutilizar ou migrar um código legado (como BAPIs ou classes existentes) para uma aplicação Fiori moderna. |

Embora o framework RAP forneça o comportamento CRUD padrão, a implementação da lógica de negócio específica da aplicação é sempre responsabilidade do desenvolvedor, tanto no modelo Managed quanto no Unmanaged.

<br>

## 🏛️ Arquitetura Técnica do RAP

Um aplicativo criado com o **RAP** conecta o banco de dados **SAP HANA** aos consumidores finais (como telas SAP Fiori ou APIs externas) por meio de uma cadeia de camadas bem organizada.  

<img width="709" height="523" alt="image" src="https://github.com/user-attachments/assets/6e920d2c-1813-4ee5-8861-3005e06262ad" />

### 🔹 Como a requisição flui

1. **SAP Gateway**  
   - Porta de entrada.  
   - Recebe chamadas OData/HTTPS e as traduz para o ambiente ABAP.  

2. **Orchestration Framework (SADL)**  
   - Decide para onde a requisição vai.  
   - Se for **leitura (Query)** → encaminha para a camada de consultas.  
   - Se for **escrita (Create, Update, Delete)** → envia para o **Business Object Framework**.  

3. **Business Object Framework (BO)**  
   - Onde vivem os **Business Objects** (entidades de negócio).  
   - Executa regras de negócio, validações e lógica transacional antes de salvar ou alterar os dados.  

4. **Query**  
   - Responsável por buscar dados no SAP HANA de forma otimizada.  
   - Usa CDS Views para entregar resultados já prontos.  

5. **SAP HANA Database**  
   - Persiste os dados.  
   - Processa consultas e cálculos diretamente em memória.  

---

### 🔹 Fluxo 

```plaintext
[ SAP Fiori UI ]         [ Web API Consumer ]
        │                        │
        └─────── OData/HTTPS ────┘
                   │
             ┌──────────────┐
             │  SAP Gateway │  ← Porta de entrada
             └───────▲──────┘
                     │
             ┌──────────────┐
             │ Orchestrator │  ← Decide o tipo (SADL)
             └───┬─────┬────┘
                 │     │
       ┌─────────┘     └─────────┐
       │                         │
┌──────────────┐         ┌──────────────┐
│ Business Obj │         │    Query     │
│  Framework   │         │ (Leituras)   │
│ (CRUD + BOs) │         └───────▲──────┘
└───────▲──────┘                 │
        │                        │
        └──────────┬─────────────┘
                   │
           ┌──────────────┐
           │   SAP HANA   │  ← Processa e armazena dados
           │ (In-Memory)  │
           └──────────────┘
```

A estrutura geral é composta por:
- **Consumidores (Consumers)**: Podem ser uma **SAP Fiori User Interface** ou um **Web API Consumer**. Ambos se comunicam com o backend através do protocolo OData via HTTPS.
- **ABAP Platform/Application Server**: Onde reside a aplicação RAP. Ele processa as requisições e interage com o banco de dados.
- **SAP HANA Database**: A base de dados onde os dados são persistidos.

<br>

## EML (Entity Manipulation Language)

A **Entity Manipulation Language (EML)** é uma extensão da linguagem ABAP criada para trabalhar com **Objetos de Negócio (Business Objects)** no RAP.  

👉 Pense nela como a forma **padronizada e simplificada** de **ler, criar, atualizar e deletar entidades RAP**, sem precisar lidar diretamente com tabelas ou SQL.  

Com EML, você pode:  
- **Ler entidades** → `READ ENTITIES`  
- **Modificar entidades** → `MODIFY ENTITIES`  

---

### 📊 Papel da EML

<img width="852" height="473" alt="image" src="https://github.com/user-attachments/assets/4162df6d-2fdb-45de-85b9-ea0f906ec04b" />

---

### 🔹 Onde a EML é usada

1. **Implementação do Behavior (Provider)**  
   - Dentro do **Behavior Implementation**, você usa EML para manipular entidades relacionadas.  
   - Aqui, sua aplicação atua como **Provedor** da lógica.  

2. **Execução de Operações (Consumer)**  
   - Um programa ABAP que precisa acessar um Business Object RAP usa EML para consumir suas funcionalidades.  
   - Aqui, sua aplicação é **Consumidor**.  

3. **Testes Unitários (ABAP Unit Test)**  
   - Em testes automáticos, a EML é usada para simular chamadas ao BO e validar o comportamento.  
   - Isso garante que a lógica funcione como esperado, sem precisar de interface gráfica ou API externa.  

---

### 🔹 Fluxo visual simplificado (ASCII)

```plaintext
                 ┌────────────────────┐
                 │ SAP Fiori / APIs   │
                 └─────────┬──────────┘
                           │ OData/HTTPS
                           ▼
                ┌─────────────────────────┐
                │ABAP RESTful Application │
                └─────────┬───────┬───────┘
                          │       │
         ┌────────────────┘       └────────────────┐
         ▼                                        ▼
┌────────────────────┐                  ┌────────────────────┐
│   ABAP Source Code │  (Consumer)      │   ABAP Unit Test   │
│   (READ/MODIFY)    │  → via EML →     │ (valida o BO via   │
└────────────────────┘                  │   EML programática)│
                                        └────────────────────┘

                 ┌────────────────────┐
                 │ Business Object BO │  (Provider)
                 │   Implementação    │
                 └─────────▲──────────┘
                           │
                      via EML
                           │
                  ┌────────────────────┐
                  │ SAP HANA Database  │
                  │   (persistência)   │
                  └────────────────────┘
```

## 🧩 Principais Artefatos de Desenvolvimento RAP

O ABAP RESTful é um modelo de programação que disponibiliza um conjunto de objetos de desenvolvimento, linguagens e APIs específicas. A interação entre esses artefatos permite implementar funcionalidades de negócio de forma estruturada. Os dois componentes iniciais e mais importantes de um Objeto de Negócio no RAP são o modelo de dados (definido com CDS) e o seu comportamento (Behavior).

<img width="927" height="824" alt="rap" src="https://github.com/user-attachments/assets/bd4a933d-fde7-4980-b841-f7bd532932c1" />

Além dos artefatos listados, o Behavior Definition é acompanhado de uma Behavior Implementation (classe ABAP), onde são codificadas as regras de negócio de forma prática, como validações e ações customizadas.

O fluxo de desenvolvimento segue normalmente de baixo para cima: começa-se pela tabela base, depois define-se a CDS View Entity (modelo de dados), em seguida o Behavior Definition + Implementation (regras de negócio) e finalmente a Projection + Metadata Extension, que expõem os dados e funcionalidades para aplicações externas como Fiori/UI5 ou serviços OData.

1. **Database Table (Active + Draft)**  
   Base física dos dados.  

2. **CDS View Entity**  
   Modelo de dados semântico.  

3. **Behavior Definition (BDL)**  
   Define regras e operações permitidas.  

4. **Behavior Implementation (ABAP)**  
   Implementa as regras de negócio.  

5. **CDS Projection View**  
   Decide o que será exposto externamente.  

6. **CDS Metadata Extension**  
   Ajusta como a UI enxerga os dados.  

7. **Fiori/UI5 ou OData Service**  
   Aplicações e usuários finais consumindo.  

---

| Linguagem (DL) | Descrição |
| :--- | :--- |
| **DDL** (Data Definition Language) | Utilizada nas CDS para definir a semântica dos modelos da aplicação e expor os dados no serviço OData. | 
| **DDLA** (Data Definition Language Annotations) | Define as anotações que servem para adicionar características aos objetos definidos na DDL. | 
| **DCL** (Data Control Language) | Linguagem que define os dados de controle de acesso. | 
| **BDL** (Behavior Definition Language) | Utilizada nos *Behaviors* para controlar as ações da aplicação. |
| **SDL** (Service Definition Language) | Utilizada nos serviços que definem a exposição dos dados via OData. |

<br>

## 🎭 Camada de Projeção (Projection Layer)

Depois de criar o **Objeto de Negócio principal**, usamos a **Camada de Projeção** para preparar uma versão simplificada e adaptada dele, voltada a um caso de uso específico (como uma tela ou API).  

👉 Pense nela como um **filtro**: mostra só os dados e operações necessários para a aplicação final.  

A camada tem **duas partes**:  

1. **CDS Projection View (dados)**  
   - Seleciona apenas os campos e associações relevantes.  
   - Adiciona anotações `@UI` para definir a aparência na interface (ex: campos de pesquisa, títulos, visibilidade).  
   - Mantém o modelo principal **limpo** e genérico.  

2. **Behavior Projection (lógica)**  
   - Define quais operações do objeto principal estarão disponíveis (ex: permitir só leitura, bloquear create/delete).  
   - Pode adicionar **ações específicas** para esse cenário, sem alterar o objeto principal.  

---

## 🚀 Camada de Serviço de Negócio (Business Service Layer)

Depois que a Projeção prepara os dados e comportamentos, a **Camada de Serviço** expõe tudo isso como uma **API** ou **serviço pronto para uso**.  

Ela tem **dois elementos**:  

1. **Service Definition (o contrato)**  
   - Lista quais entidades e operações da projeção farão parte do serviço.  
   - Responde: **“O que meu serviço oferece?”**  

2. **Service Binding (a publicação)**  
   - Define **como** o serviço será acessado (ex: OData V2 ou OData V4).  
   - Especifica o **tipo de consumidor** (UI Fiori ou API externa).  
   - Ativa o serviço e gera sua **URL** para uso.  

---

## 🌟 Requisitos Não Funcionais e Qualidades da Arquitetura RAP

Na arquitetura de software, os requisitos não funcionais e os atributos de qualidade, como **desempenho, usabilidade, flexibilidade e adaptabilidade**, desempenham um papel fundamental. Eles determinam a usabilidade a longo prazo e a longevidade de um produto de software. A arquitetura do ABAP RAP foi projetada com recursos de qualidade específicos para atender a esses requisitos.

### Adaptabilidade e Desacoplamento da Tecnologia

O desenvolvimento de software moderno busca criar soluções capazes de evoluir com o tempo para se adaptar a novas tendências de mercado, como uma nova tecnologia de UI ou uma nova forma de armazenar dados. Um software é considerado adaptável quando sua lógica de negócios não está misturada com a codificação técnica.

O RAP alcança esse desacoplamento de forma nativa:
-   As propriedades transacionais e a lógica de negócios são declaradas na **Behavior Definition** sem a necessidade de conhecer a implementação técnica concreta.
-   O protocolo técnico de exposição do serviço (como OData V2 ou V4) não é referenciado até a etapa final do **Service Binding**.
-   Isso torna o Service Binding um ponto de ancoragem para a adaptação de novas tecnologias e protocolos de interface no futuro, sem impactar a lógica de negócio principal.

### Abordagem Declarativa

A capacidade de evolução do RAP é suportada por sua abordagem declarativa de ponta a ponta.
-   **Modelo de Dados**: Graças às CDS, os modelos de dados são declarados em um nível puramente lógico e, geralmente, são independentes da fonte de dados física.
-   **Interface do Usuário (UI)**: As anotações de UI também são declaradas em um nível lógico. Elas especificam a funcionalidade desejada sem precisar saber a implementação concreta na UI. Isso permite que as anotações permaneçam inalteradas mesmo que a tecnologia de UI usada para interpretá-las mude no futuro.

### Encapsulamento de Aplicações Legadas

A capacidade evolutiva do RAP beneficia não apenas aplicações novas ("greenfield"), mas também aplicativos existentes que são encapsulados com o modelo.
-   Aplicações legadas encapsuladas podem ser expostas a várias interfaces e protocolos usando RAP, sem que isso tenha sido definido no aplicativo legado original.
-   Isso garante que mesmo aplicações existentes possam se beneficiar de novas funcionalidades ou interfaces técnicas que venham a ser suportadas no futuro.

<br>

## 🖥️ Ambiente de Desenvolvimento: ABAP Development Tools (ADT)

Ao construir aplicativos com o ABAP RESTful Application Programming Model, o uso do **ABAP Development Tools (ADT)** é mandatório.
-   O ADT é um Toolkit que fornece a perspectiva ABAP dentro da ferramenta Eclipse, instalado localmente no computador do desenvolvedor.
-   Com ele, é possível criar qualquer objeto do Dicionário ABAP, analisar performance, visualizar diagramas de relacionamento, realizar debug, entre muitas outras funções.
-   Todas as ferramentas necessárias para desenvolvimento, teste e análise de aplicações RAP estão disponíveis neste ambiente.

Uma única instalação do ADT pode se conectar a múltiplos sistemas ABAP de diferentes versões, sejam eles sistemas locais (On-Premise), sistemas baseados na SAP Business Technology Platform (SAP BTP) ou sistemas SAP S/4HANA Cloud. O escopo funcional disponível no ADT sempre dependerá da versão do sistema back-end ao qual ele está conectado.

### Boas práticas na nomenclatura dos objetos 

| Object Type | Naming |
| :--- | :--- |
| Database Table (Active data) | `Z<Name>` |
| Database Table (Draft data) | `Z<Name>_D` |
| CDS View Entity (Model) | `ZR_<Name>` |
| CDS View Entity (Projection) | `ZC_<Name>` |
| CDS Metadata Extension | `ZC_<Name>` |
| CDS Behavior Definition (Model) | `ZR_<Name>` |
| CDS Behavior Definition (Projection) | `ZC_<Name>` |
| Global Class (Behavior Implementation) | `ZBP_R<Name>` |
| Service Definition | `ZUI_<Name>_O4` |
| Service Binding | `ZUI_<Name>_O4` |

<br>

## 🔗 Visão Geral da Relação entre os Objetos

O diagrama a seguir consolida a relação entre os principais artefatos de desenvolvimento do RAP, mostrando o fluxo desde a modelagem dos dados até a exposição do serviço de negócio.

A estrutura é dividida em camadas lógicas:

-   **Camada de Modelagem de Dados e Comportamento (Data Modeling and Behavior)**: A base da aplicação.
    -   `CDS-Based Data Model`: Define a estrutura das entidades de negócio.
    -   `Behavior Definition`: Declara o comportamento transacional do modelo de dados.
    -   `Behavior Implementation`: Implementa a lógica de negócio em ABAP.
-   **Camada de Projeção (Projection Layer)**: Esta camada é **opcional** e serve para criar uma visão específica do objeto de negócio para um determinado cenário.
    -   `CDS Projection Views`: Filtra e anota os campos do modelo de dados.
    -   `Behavior Projection`: Restringe ou adiciona comportamentos específicos para a projeção.
    -   `Behavior Implementation`: Implementa a lógica do comportamento da projeção.
-   **Camada de Exposição (Exposing Business Services)**: A camada final que torna o serviço acessível.
    -   `Service Definition`: Define quais entidades da projeção (ou do modelo base) serão expostas.
    * `Service Binding`: Define o protocolo (OData V2/V4) e ativa o serviço.
 
## 🚀 Eficiência no Desenvolvimento com RAP

Um dos principais requisitos de qualidade de um modelo de programação é a eficiência na construção de aplicativos. Um esforço contínuo no desenvolvimento de software é aliviar a carga das equipes de desenvolvimento, oferecendo tecnologias e serviços transversais (como servidores de aplicação e tecnologias de UI) para que elas possam se concentrar na implementação da camada de negócios.

O RAP atinge essa eficiência através de um princípio fundamental: a **separação entre negócios e tecnologia**, que é uma aplicação do princípio da "separação de interesses". Isso é essencial para desenvolver uma lógica de negócios que seja independente da tecnologia externa, que possa ser alterada independentemente da tecnologia e que seja reutilizável.

O modelo RAP implementa este princípio através de quatro abordagens principais:

### 1. Abordagem Declarativa e Baseada em Modelo

Uma abordagem baseada em modelo fornece uma linguagem formal adaptada a um propósito específico, como a BDL (Behavior Definition Language) para modelagem de comportamento.
-   Com esta abordagem, você **declara a funcionalidade desejada ("o quê")** e não precisa se preocupar com as especificações técnicas concretas da implementação ("o como").
-   Os objetos de desenvolvimento são gerados a partir do código-fonte, e o modelo é avaliado genericamente em tempo de execução.

### 2. Integração com a Linguagem ABAP

O modelo de programação RAP é totalmente integrado à linguagem ABAP.
-   Baseado no modelo de dados CDS, essa integração garante a escrita de interfaces estritamente tipadas.
-   Isso permite a verificação de sintaxe estática e um melhor suporte de ferramentas no ADT, como assistência de código e informações de elementos.
-   Como resultado, a suscetibilidade a erros e a quantidade de pesquisa necessária são reduzidas, o que beneficia diretamente a eficiência do desenvolvimento.

### 3. Implementações Padrão (Standard Implementations)

O modelo RAP fornece implementações padrão para tarefas comuns de desenvolvimento, como operações CRUD, gerenciamento de buffer de transação, persistência e tratamento de rascunhos.
-   É possível usar essas implementações padrão simplesmente especificando palavras-chave apropriadas na Behavior Definition, o que permite desenvolver rapidamente as primeiras versões de um novo aplicativo.
-   A implementação padrão do OData, por exemplo, é fornecida pelo SAP Gateway em segundo plano, e o desenvolvedor pode acessá-la via Service Binding sem nem precisar saber que o Gateway é a tecnologia por trás dela.

### 4. Vocabulário Padrão para Aplicações de Negócio

O RAP define um vocabulário padrão para a lógica de negócios.
-   Termos como **validações (validations), determinações (determinations) e ações (actions)** podem ser usados como palavras-chave explícitas na Behavior Definition e, portanto, estão incluídos na linguagem ABAP.
-   O uso desses conceitos padronizados facilita a comunicação dentro da equipe de desenvolvimento e com especialistas de negócio ao discutir os requisitos e sua implementação.

<br>

## ✅ Testabilidade e Mutabilidade

A **mutabilidade**, ou a capacidade de um software ser alterado com segurança, foi garantida desde os primórdios do desenvolvimento ágil, principalmente por meio de testes automatizados. O código de um programa é muito mais fácil e seguro de alterar se for apoiado por casos de teste confiáveis criados com ferramentas de automação.

O recurso de **testabilidade** de um software determina a facilidade com que sua funcionalidade pode ser testada automaticamente por uma suíte de testes. O RAP foi projetado para facilitar essa automação.

-   O RAP introduz os conceitos da **fase de interação** e do **buffer de transação**, e o modelo torna difícil "programar além" desses conceitos, garantindo um fluxo previsível e testável.
-   Os testes em RAP são habilitados pela implementação de casos de teste usando o **Framework ABAP Unit**.
-   O ABAP Unit é um framework de testes que permite desenvolver e executar testes de unidade para as aplicações, garantindo a qualidade e a segurança nas alterações.
