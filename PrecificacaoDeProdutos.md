# Documentação Técnica – Sistema de Precificação de Produtos

## 1. Visão Geral

Este documento descreve de forma estruturada como o sistema realiza o **cálculo do preço final de venda dos produtos**, detalhando critérios, hierarquia de regras, entidades envolvidas e comportamentos especiais. O objetivo é servir como referência técnica e funcional, especialmente para manutenção, parametrização e entendimento do fluxo de precificação.

---

## 2. Conceito Central

**Precificação → Preço Final de Venda**

O preço final de um produto é resultado da aplicação de múltiplos critérios configuráveis, avaliados de forma hierárquica, envolvendo:

* Preços base
* Tabelas de preço
* Tipos de movimento
* Regras por produto, pessoa e promoção
* Condicionais implementadas em SQL

---

## 3. Preço Base

### 3.1 Cadastro

* É possível cadastrar até **8 preços base** por produto.
* Um dos preços é considerado **preço padrão** (campo obrigatório).
* Todos os preços base podem ser utilizados por **todas as lojas da empresa**.

### 3.2 Comportamento para Valores Zerados

* Caso algum preço opcional esteja **zerado**, o sistema assume automaticamente o **preço padrão**.
* Para evitar a utilização de valores zerados:

  * Na **Tabela de Preço → Extra**, existe a opção:

    * **“Buscar preço 1 se preço base zerado”**
  * Essa opção vem **marcada por padrão**.
  * Se desmarcada, o sistema utilizará o valor zerado explicitamente.

---

## 4. Tipo de Movimento

### 4.1 Definição de Preço por Movimento

* O **Tipo de Movimento (código 37)** está associado a uma **Tabela de Preço**.
* Essa associação define **qual valor será utilizado** para o produto em um movimento específico.
* A definição do tipo de movimento também pode variar **por loja**, funcionando como critério adicional.

### 4.2 Restrições

* É possível **restringir** quais tabelas de preço podem ser usadas com determinados tipos de movimento.
* Essa configuração é feita diretamente no **cadastro do tipo de movimento**.

---

## 5. Tabela de Preço

### 5.1 Função Principal

* A **Tabela de Preço (código 27)** define:

  * A **base de cálculo** do preço
  * Qual **loja ou empresa** deve utilizá-la

### 5.2 Características

* A tabela de preço é considerada **linear** (não possui validade por data).
* Cada item pode ter sua própria tabela de preço.
* Para facilitar a operação:

  * Pode-se atribuir **uma única tabela de preço a todos os itens**.
  * Mesmo assim, é obrigatório definir a **tabela de preço padrão no tipo de movimento**.

---

## 6. Regras de Precificação

### 6.1 Regras com Preço Fixo

* A tabela de preço permite definir **regras de preço fixo** para produtos.
* Quando aplicadas:

  * O **preço base é ignorado**.

### 6.2 Combinações de Regras

* Existem **7 combinações diferentes de regras**, que:

  * Englobam múltiplos produtos
  * Criam um processo de **afunilamento extremo** para definição do preço final

### 6.3 Controle Granular por Produto

* É possível definir um **percentual dinâmico por produto**, permitindo ajustes finos individuais.

### 6.4 Regras por Pessoa

* Caso não exista regra específica para o produto:

  * O sistema pode buscar **regras atribuídas às pessoas (clientes)**.

---

## 7. Pessoas (Clientes)

* No cadastro de **Pessoas (código 5)**:

  * Pode ser definida uma **Tabela de Preço Padrão**.
* Essa tabela é utilizada quando não há regras mais específicas aplicáveis.

---

## 8. Agenda de Promoção

### 8.1 Conceito

* A **Agenda de Promoção (código 63)** adiciona regras temporárias ao cálculo de preço.

### 8.2 Características

* Possui **data de validade**.
* Pode conter restrições como:

  * “Somente para pagamento à vista”.
* É possível definir **uma ou mais tabelas de preço** dentro da promoção.

### 8.3 Hierarquia

* A promoção é o **último parâmetro avaliado** no cálculo.
* Representa o **menor nível de prioridade** na hierarquia de decisão.

---

## 9. Hierarquia Simplificada de Decisão

1. Preço Base (e validação de zerados)
2. Tabela de Preço associada ao Tipo de Movimento
3. Regras de preço fixo
4. Regras por produto (percentual dinâmico)
5. Regras por pessoa
6. Tabela de preço padrão da pessoa
7. Agenda de promoção (último nível)

---

## 10. Implementação Técnica

### 10.1 Regras de Combinação

* As regras de precificação são implementadas diretamente em **SQL**.
* Utilizam **condicionais**, o que:

  * Facilita a manutenção
  * Reduz impacto de mudanças estruturais

### 10.2 Código-Fonte

* A lógica de cálculo pode ser analisada no arquivo:

  * **`uDalCalculoProduto.pas`**

---

## 11. Considerações Finais

O sistema de precificação é altamente flexível e modular, permitindo:

* Múltiplos níveis de configuração
* Ajustes finos por produto, cliente e contexto
* Evolução controlada por regras condicionais

Essa complexidade exige atenção especial à hierarquia e às configurações padrão para evitar comportamentos inesperados no preço final.
