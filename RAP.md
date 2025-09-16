# ABAP RESTful Application Programming Model (RAP)

Este documento reúne conceitos, explicações e exemplos práticos sobre o uso do **ABAP RESTful Application Programming Model (RAP)**, a arquitetura padrão da SAP para o desenvolvimento de aplicações Fiori e Web APIs otimizadas para SAP HANA, tanto no ambiente On-Premise quanto na nuvem (BTP).

O **ABAP RAP** representa a evolução do desenvolvimento ABAP, fornecendo um modelo de programação que padroniza a criação de serviços OData de ponta a ponta. Com o S/4HANA, modificações tecnológicas fundamentais foram introduzidas no ABAP, impactando todas as camadas do desenvolvimento, desde o banco de dados SAP HANA até a experiência do usuário com SAP Fiori e SAPUI5.

- **Desenvolvimento Eficiente**: Reduz a complexidade ao fornecer frameworks que automatizam tarefas técnicas.
- **Consistência e Padronização**: Garante que as aplicações sigam uma arquitetura consistente e um fluxo de desenvolvimento padronizado.
- **Otimizado para SAP HANA**: Tira proveito do "Code Pushdown", levando o código ao nível do banco de dados, sem a necessidade de mover os dados para a memória principal do servidor de aplicação. A tecnologia in-memory do HANA armazena os dados na memória principal do banco e utiliza um armazenamento em colunas com indexação automática para acelerar o acesso.
- **Baseado em REST**: As APIs geradas seguem os princípios REST, garantindo interoperabilidade e escalabilidade.

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

O **Open Data Protocol (OData)** é o padrão utilizado pelo RAP para expor os serviços. Ele é uma especificação de API REST gerenciada pela OASIS que define como os dados são trocados entre o cliente e o back-end.

Uma URL OData é composta por três partes principais:

  - **Service Root URL**: O endereço base do serviço.
  - **Resource Path**: O caminho para a entidade específica que está sendo acessada.
  - **Query Options**: Parâmetros para filtrar, ordenar e paginar os dados (ex: `$top`, `$orderby`).

### Anotações e Vocabulários

Embora o OData defina a estrutura de acesso, os **OData Vocabularies** e as **Annotations** enriquecem o modelo de dados com semântica adicional, que é interpretada pela interface do usuário (UI). Por exemplo, uma anotação pode definir que o título de uma tela de "Pessoa" deve ser composto pelos campos `LastName` e `FirstName`.

### Métodos HTTP e Operações CRUD

O RAP mapeia as operações CRUD (Create, Read, Update, Delete) para os métodos HTTP padrão do protocolo REST.

| Método | Operação CRUD | Descrição | Código de Sucesso Comum |
| :--- | :--- | :--- | :--- |
| **GET** | Read | Representa a leitura dos dados. | `200` (OK), `404` (Not Found) |
| **POST** | Create | Representa a criação de novos dados. | `201` (Created), `204` (No Content) |
| **PUT** | Update | Modifica uma entidade inteira. | `200` (OK), `204` (No Content) |
| **PATCH** | Update | Modifica apenas atributos específicos de uma entidade. | `200` (OK), `204` (No Content) |
| **DELETE**| Delete | Representa a deleção dos dados. | `200` (OK), `204` (No Content) |

### OData V4: Mais Eficiência

O OData V4, padronizado pela OASIS e ISO/IEC, é a versão recomendada pela SAP e traz diversas vantagens sobre o V2:

  - **Melhor compactação de metadados**, economizando volume de dados.
  - **Consultas mais sofisticadas** e expansões de vários níveis, reduzindo o número de chamadas.
  - **Recursos analíticos avançados**.
  - Acesso a múltiplos serviços simultaneamente pelo client.

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

<br>

## 🛠️ Tipos de Implementação: Managed vs. Unmanaged

O modelo RAP introduz o conceito de **Implementation Type** para especificar quem é responsável por fornecer a implementação da lógica de leitura (Query) e de escrita (Behavior). A escolha entre os dois tipos define como o comportamento da aplicação será desenvolvido.

| Tipo | Descrição | Cenário de Uso |
| :--- | :--- | :--- |
| **Managed (Gerenciado)** | Utiliza a implementação pronta do RAP. O framework fornece automaticamente o comportamento CRUD padrão para as CDSs e gerencia o buffer transacional. A funcionalidade fica disponível sem programação adicional, mas o desenvolvedor pode adicionar novas lógicas para complementar o comportamento padrão. | Ideal para cenários "Greenfield" (desenvolvimento de novas aplicações), onde não há lógica legada a ser reutilizada e se busca máxima aceleração no desenvolvimento. |
| **Unmanaged (Não Gerenciado)** | O desenvolvedor precisa implementar todas as funcionalidades, incluindo a fase de interação com o buffer. Este tipo permite integrar APIs e lógicas de negócio já existentes, envolvendo-as no modelo RAP. | Frequentemente utilizado em cenários "Brownfield", onde é necessário reutilizar ou migrar um código legado (como BAPIs ou classes existentes) para uma aplicação Fiori moderna. |

É importante notar que, além do comportamento CRUD padrão, as aplicações quase sempre possuem requisitos de negócio muito específicos que o framework não pode fornecer. Nesses casos, tanto no modelo **Managed** quanto no **Unmanaged**, é possível e esperado que o desenvolvedor implemente a respectiva lógica de negócios customizada.

<br>

## 🏛️ Arquitetura Técnica do RAP

Um aplicativo construído com o RAP possui uma arquitetura bem definida que conecta a base de dados aos consumidores finais, como interfaces Fiori ou APIs web.

A estrutura geral é composta por:
- **Consumidores (Consumers)**: Podem ser uma **SAP Fiori User Interface** ou um **Web API Consumer**. Ambos se comunicam com o backend através do protocolo OData via HTTPS.
- **ABAP Platform/Application Server**: Onde reside a aplicação RAP. Ele processa as requisições e interage com o banco de dados.
- **SAP HANA Database**: A base de dados onde os dados são persistidos.

Dentro do Application Server, o fluxo de uma requisição passa por várias camadas, conforme detalhado no diagrama de arquitetura.

### Fluxo da Requisição: Gateway e Orquestrador

1.  **SAP Gateway**: Atua como o portão de entrada. Ele implementa o protocolo OData, recebe as requisições externas e as encaminha para o Orquestrador para processamento.
2.  **Orchestration Framework**: Avalia a requisição de forma genérica. A linguagem utilizada por ele é a **SADL (Service Adaptation and Description Language)**. O Orquestrador direciona a requisição da seguinte forma:
    - **Requisições de Leitura**: São encaminhadas para a **Query** apropriada, que é responsável por buscar os dados no banco.
    - **Requisições de Escrita (CRUD)**: São processadas via **Business Object Framework**, que lida com a lógica transacional.

<br>

## 🗣️ EML (Entity Manipulation Language)

A **Entity Manipulation Language (EML)** é uma nova sintaxe, parte integral da linguagem ABAP, utilizada no contexto do RAP para manipular os dados que vêm da camada de negócio (CDSs, etc.). É a tipagem padronizada para acessar os dados e as funcionalidades de um Objeto de Negócio RAP.

Com EML, é possível ler instâncias com a instrução `READ ENTITIES` e modificar instâncias com `MODIFY ENTITIES`.

A EML desempenha um papel crucial em diferentes casos de uso:

- **Implementação do Behavior**: É possível utilizar instruções EML dentro da implementação do comportamento de um Objeto de Negócio. Neste cenário, a sua aplicação assume o papel de **Provedor (Provider)** da lógica.
- **Execução de Operações (Consumo)**: É necessário programar com instruções EML quando um aplicativo ABAP precisa acessar a funcionalidade de um Objeto de Negócio RAP. Nesse caso, o aplicativo atua como **Consumidor (Consumer)**.
- **Testes Unitários**: A EML é usada no contexto de testes unitários (ABAP Unit Test) para verificar a funcionalidade de um Objeto de Negócio de forma programática e automatizada.

## 🧩 Principais Artefatos de Desenvolvimento RAP

O ABAP RESTful é um modelo de programação que disponibiliza um conjunto de objetos de desenvolvimento, linguagens e APIs específicas. A interação entre esses artefatos permite implementar funcionalidades de negócio de forma estruturada. Os dois componentes iniciais e mais importantes de um Objeto de Negócio no RAP são o modelo de dados (definido com CDS) e o seu comportamento (Behavior).

### Modelo de Dados com Core Data Services (CDS)

Cada aplicativo RAP é baseado em um modelo de dados que representa suas entidades de negócio, descreve seus atributos e mapeia os relacionamentos (associações) com outras entidades.

-   **Entidade de Negócio**: Representa um conceito de negócio. Por exemplo, um mestre de materiais ou um pedido de vendas são objetos de negócio típicos.
-   **Modelo Lógico com CDS**: Para definir o modelo lógico de dados de um aplicativo, utiliza-se Core Data Services (CDS). Para cada entidade de negócio, uma entidade CDS correspondente é criada no sistema.
-   **Fonte de Dados**: Normalmente, as entidades CDS são criadas com base em tabelas transparentes existentes, mas também podem ser definidas de forma independente, sem uma fonte de dados preexistente.

#### A Entidade Raiz (CDS Root Entity)

O conceito de um objeto de negócios com suas entidades CDS dependentes é de particular importância no RAP. Uma transação ou objeto de negócio completo (como um Pedido de Vendas com seus itens, parceiros e categorias) é mapeado em sua totalidade para uma entidade CDS especial chamada **entidade raiz (ROOT)**. Esta entidade raiz serve como o ponto de entrada principal para o objeto de negócio e governa todas as suas entidades filhas (dependentes), formando uma estrutura hierárquica.

### O Modelo de Comportamento (Behavior)

Enquanto o modelo de dados CDS define a estrutura, o **Behavior** de um objeto de negócios define sua lógica transacional. O Behavior agrupa operações de gravação (como criação e modificação), propriedades transacionais (como bloqueios e autorizações) e lógica de negócios interna (como verificações e cálculos).

O Business Object é o responsável por controlar o processamento das operações durante as fases de `Interaction` e `Save Sequence`, e os métodos para isso são chamados pela camada do Behavior.

#### Behavior Definition e a Linguagem BDL

Para definir o comportamento de um objeto de negócios, cria-se um **Behavior Definition**, que é um novo tipo de objeto de desenvolvimento no RAP.
-   Ele é criado com referência à entidade **root** da CDS e, consequentemente, seu escopo se aplica a todas as entidades subordinadas a ela.
-   O comportamento é declarado usando a **Behavior Definition Language (BDL)**. Por exemplo, pode-se usar as palavras-chave `create`, `update` ou `delete` para especificar que a entidade suporta as operações padrão correspondentes.
-   É também na Behavior Definition que se especifica o tipo de implementação (**Managed** ou **Unmanaged**) para a funcionalidade do objeto de negócios.

### Detalhando a Implementação do Behavior

Dentro da classe ABAP do **Behavior Pool**, a implementação é dividida em diferentes categorias de lógica, cada uma com um propósito específico.

#### Operações Padrão (Standard Operations)

Incluem as operações básicas de **criação, leitura, atualização e exclusão** de dados.
-   Para que estas operações funcionem, elas devem ser implementadas adequadamente no contexto do modelo transacional, que inclui a fase de interação, o buffer de transação e a sequência de salvamento.
-   Por exemplo, a operação `create` requer que a nova instância seja primeiro adicionada ao buffer de transação.
-   A implementação dessas operações pode ser fornecida pelo framework (cenário **Managed**) ou executada pelo próprio desenvolvedor (cenário **Unmanaged**).

#### Operações Específicas (Specific Operations)

Existem para a implementação de **ações ou funções** customizadas e relacionadas ao aplicativo (por exemplo, um botão "Aprovar Pedido").
-   Estas operações **sempre exigem uma implementação de Behavior própria**.

#### Lógica de Negócio Interna (Internal Business Logic)

Refere-se à implementação de **validações ou determinações** que são visíveis apenas dentro do objeto de negócios.
-   Exemplos incluem calcular um campo com base em outro (determinação) ou verificar se um valor é permitido (validação).
-   Assim como as operações específicas, esta lógica **sempre precisa de uma implementação de comportamento própria**.

#### Comportamento Transacional (Transactional Behavior)

Geralmente, é necessária uma implementação própria de Behavior para aspectos transacionais como **verificações de autorização, numeração (Numbering) e bloqueios (Locks)**.
-   No entanto, para a atribuição de números, é possível utilizar uma implementação pronta do framework, como a numeração gerenciada com **GUIDs (identificadores exclusivos globais)**.
-   GUIDs podem ser usados para atribuir valores-chave às novas instâncias.

<br>

## 🎭 A Camada de Projeção (Projection Layer)

Após a definição do Objeto de Negócio principal (com seu modelo de dados e comportamento), a **Camada de Projeção** é utilizada para adaptar este objeto para um caso de uso específico. O objetivo geral é **limitar os atributos e operações** que serão expostos ao consumidor final.

A camada de projeção é composta por duas partes:

-   **Projeção de CDS (CDS Projection View)**: Uma visão de projeção CDS é usada para declarar quais atributos e associações do modelo de dados principal são relevantes para o caso de uso. Nela, é possível enriquecer o modelo com anotações específicas para a UI, como a definição de uma interface de pesquisa ou a configuração de um campo para ajuda de entrada.
-   **Projeção de Comportamento (Behavior Projection)**: É um tipo especial de implementação que restringe as operações e comportamentos declarados na Behavior Definition principal. Além de restringir, você também pode adicionar operações adicionais na projeção, implementando-as em ABAP, sem ter que alterar a funcionalidade do objeto de negócios principal. Isso exige uma implementação de comportamento adicional na própria camada de projeção.

<br>

## 🚀 A Camada de Serviço de Negócio (Business Service Layer)

Esta é a camada final, usada para expor os objetos de negócios RAP (seus dados e comportamentos, já adaptados pela camada de projeção) como uma API remota para ser consumida externamente. Ela é composta por dois artefatos:

### Definição de Serviço (Service Definition)

O Service Definition contém as entidades CDS que serão expostas a partir do modelo de dados da aplicação.
-   Ele determina exatamente quais dados e qual comportamento são expostos como um serviço.
-   Normalmente, a exposição é feita com base nas visões de projeção (e projeções de comportamento), mas em casos mais simples, pode ser feita diretamente com base nas entidades CDS do objeto de negócios principal.

### Vinculação de Serviço (Service Binding)

Baseado em um Service Definition, o Service Binding permite definir o protocolo técnico específico através do qual o serviço será exposto.
-   Pode ser, por exemplo, **OData versão 2 (OData V2) ou OData versão 4 (OData V4)**.
-   Para cada protocolo, é feita uma distinção entre uma variante para **consumidores de UI** e uma para **consumidores de API web**.
-   O Service Binding também oferece suporte ao controle de versão da interface e é o local onde autorizações padrão podem ser atribuídas.

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

## 🌎 Disponibilidade do ABAP RAP

Nesta seção, veremos em quais produtos SAP o ABAP RESTful Application Programming Model está disponível e como ele se posiciona em cada caso.

O modelo RAP cria um espaço explícito para a lógica de negócios através do conceito de Objeto de Negócio, com sua definição de comportamento e implementação. Isso torna difícil incorporar codificação técnica que afete o processo de desenvolvimento, reforçando o princípio de que a lógica de negócio deve ser separada da lógica técnica. Por exemplo, o cálculo do melhor preço de compra (lógica de negócio) deve ser independente de como esse preço será exibido na UI ou transmitido para um sistema de terceiros (lógica técnica). É essa separação que permite que o RAP seja implantado em diferentes plataformas tecnológicas.

### Em Ambientes On-Premise (SAP S/4HANA)

-   **Lançamento**: O modelo de programação RAP on-premise foi disponibilizado pela primeira vez com a plataforma ABAP para **SAP S/4HANA 1909 FPS00** (equivalente ao SAP NetWeaver 7.54).
-   **Evolução**: O RAP substituiu o "ABAP Programming Model for SAP Fiori" (introduzido no SAP NetWeaver 7.50), que, no entanto, continua a ser totalmente apoiado.
-   **Ciclo de Lançamento**: A plataforma ABAP não é mais fornecida individualmente, mas sim como parte de uma instalação do SAP S/4HANA. Consequentemente, a plataforma ABAP está vinculada ao **ciclo anual de lançamento** do SAP S/4HANA on-premise, o que significa que novos recursos do RAP ficam disponíveis apenas uma vez por ano.
-   **Desenvolvimento**: Embora o princípio do "Clean Core" se aplique, no ambiente on-premise os desenvolvedores tecnicamente têm a opção de usar objetos de desenvolvimento SAP não lançados e as verificações de API não são impostas sintaticamente. Modificações no sistema também
