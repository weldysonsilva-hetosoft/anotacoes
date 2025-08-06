# SQL - Conceitos com Prática

Este material é voltado para iniciantes que desejam aprender SQL com profundidade, do absoluto zero até comandos intermediários e avançados. Tudo com explicações detalhadas, exemplos reais e exercícios contextualizados para uso profissional.

---

## 📘 Parte 1 – Fundamentos do SQL e comandos DDL, DML e DQL

### 🔍 O que é SQL?

SQL (Structured Query Language) é a linguagem padrão para trabalhar com banco de dados relacionais. Com ela você pode:

- Criar estruturas (tabelas, colunas, chaves)
- Inserir, atualizar e deletar dados
- Consultar e filtrar informações
- Relacionar dados entre tabelas

### 📌 Tipos de comandos SQL

| Categoria | Nome | Ação principal | Exemplos |
| --- | --- | --- | --- |
| DDL | Data Definition Language | Criação de estrutura | CREATE, ALTER, DROP |
| DML | Data Manipulation Language | Manipular os dados | INSERT, UPDATE, DELETE |
| DQL | Data Query Language | Consulta de dados | SELECT |
| TCL | Transaction Control Language | Controle de transações | COMMIT, ROLLBACK |
| DCL | Data Control Language | Permissões | GRANT, REVOKE |

### 🧱 Criando uma tabela (DDL explicada)

```sql
CREATE TABLE TB_CLIENTE (
    ClienteId INT PRIMARY KEY IDENTITY(1,1),
    Nome VARCHAR(100) NOT NULL,
    Email VARCHAR(100),
    DataCadastro DATETIME DEFAULT GETDATE()
);

```

**Explicando linha por linha:**

- `ClienteId INT`: cria a coluna que armazenará o número de identificação do cliente.
- `PRIMARY KEY`: define que essa coluna será única e obrigatória (sem repetir).
- `IDENTITY(1,1)`: o valor começa em 1 e aumenta automaticamente de 1 em 1.
- `VARCHAR(100)`: permite texto de até 100 caracteres.
- `NOT NULL`: essa coluna é obrigatória.
- `DEFAULT GETDATE()`: insere a data atual automaticamente.

### ✍️ Inserindo dados (DML)

```sql
INSERT INTO TB_CLIENTE (Nome, Email)
VALUES ('Maria da Silva', 'maria@email.com');

```

**Explicando:**

- `INSERT INTO`: indica onde você vai inserir os dados
- `(Nome, Email)`: as colunas a preencher
- `VALUES`: os valores correspondentes

### 🔎 Consultando dados (DQL)

```sql
SELECT * FROM TB_CLIENTE;
SELECT Nome, Email FROM TB_CLIENTE;
SELECT TOP 5 * FROM TB_CLIENTE;

```

- `SELECT *`: mostra todas as colunas da tabela
- `TOP 5`: retorna só os 5 primeiros resultados
- `WHERE`, `ORDER BY` virão em partes seguintes

### 🛠️ Atualizando dados

```sql
UPDATE TB_CLIENTE
SET Email = 'novo@email.com'
WHERE ClienteId = 1;

```

- `UPDATE`: diz qual tabela atualizar
- `SET`: define o novo valor
- `WHERE`: condição para limitar quais linhas serão alteradas

### 🗑️ Deletando dados

```sql
DELETE FROM TB_CLIENTE
WHERE ClienteId = 3;

```

- `DELETE FROM`: remove dados da tabela
- `WHERE`: evita deletar todos os registros por engano

---

## 🔍 Parte 2 – Consultas com Filtros Avançados

### WHERE – Filtrando registros

```sql
SELECT * FROM TB_PRODUTO WHERE Preco > 100;

```

- Retorna somente os produtos com preço maior que 100.
- Pode combinar com `AND`, `OR`, `NOT`:

```sql
SELECT * FROM TB_PRODUTO WHERE Preco > 100 AND Estoque < 50;

```

### LIKE – Busca por pedaços de texto

```sql
SELECT * FROM TB_PRODUTO WHERE Nome LIKE '%camisa%';

```

- `%` significa “qualquer coisa” (zero ou mais letras)
- Ex: '%azul' → termina com azul | 'camisa%' → começa com camisa

### BETWEEN – Busca por intervalo

```sql
SELECT * FROM TB_PRODUTO WHERE Preco BETWEEN 50 AND 150;

```

- Inclui os valores 50 e 150

### IN – Busca por conjunto de valores

```sql
SELECT * FROM TB_PRODUTO WHERE ProdutoId IN (1, 2, 5);

```

- Equivalente a usar vários OR juntos

### ORDER BY – Ordenar resultados

```sql
SELECT * FROM TB_PRODUTO ORDER BY Preco DESC;

```

- `ASC`: crescente (padrão)
- `DESC`: decrescente

### TOP – Limita a quantidade de resultados

```sql
SELECT TOP 3 * FROM TB_PRODUTO ORDER BY Estoque DESC;

```

---

## 🔗 Parte 3 – JOINs e Relacionamentos

### INNER JOIN – Registros que existem nas duas tabelas

```sql
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
INNER JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;

```

- Retorna apenas clientes com pedidos registrados.

### 🧠 Como interpretar a query?

```sql
sql
CopiarEditar
SELECT C.Nome, P.Valor

```

- Mostra o nome do cliente vindo de `TB_CLIENTE` (`C.Nome`)
- Mostra o valor do pedido vindo de `TB_PEDIDO` (`P.Valor`)

```sql
sql
CopiarEditar
FROM TB_CLIENTE C

```

- Usa `C` como apelido para `TB_CLIENTE`

```sql
sql
CopiarEditar
INNER JOIN TB_PEDIDO P

```

- Usa `P` como apelido para `TB_PEDIDO`

```sql
sql
CopiarEditar
ON C.ClienteId = P.ClienteId;

```

- Faz a ligação entre os dois usando `ClienteId` (a chave estrangeira)

---

### 🧪 Exemplo prático para entender

Imagine essas tabelas:

**TB_CLIENTE**

| ClienteId | Nome |
| --- | --- |
| 1 | João |
| 2 | Maria |

**TB_PEDIDO**

| PedidoId | ClienteId | Valor |
| --- | --- | --- |
| 10 | 1 | 300 |
| 11 | 1 | 200 |
| 12 | 2 | 150 |

Resultado da query:

```sql
sql
CopiarEditar
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
INNER JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;

```

| Nome | Valor |
| --- | --- |
| João | 300 |
| João | 200 |
| Maria | 150 |

### LEFT JOIN – Todos os da esquerda, mesmo sem correspondência

```sql
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
LEFT JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;

```

- Mostra todos os clientes, mesmo os sem pedidos (P.Valor será NULL).

### RIGHT JOIN – Todos da direita, com ou sem cliente

```sql
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
RIGHT JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;

```

### FULL JOIN – Traz todos os dados, mesmo sem correspondência

```sql
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
FULL JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;

```

### CROSS JOIN – Produto cartesiano (cada um com todos)

```sql
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
CROSS JOIN TB_PEDIDO P;

```

- CUIDADO: pode gerar milhares ou milhões de linhas

### UPDATE com JOIN

```sql
UPDATE P
SET P.Valor = P.Valor * 0.9
FROM TB_PEDIDO P
INNER JOIN TB_CLIENTE C ON P.ClienteId = C.ClienteId
WHERE C.Nome LIKE '%João%';

```

### DELETE com JOIN

```sql
DELETE P
FROM TB_PEDIDO P
INNER JOIN TB_CLIENTE C ON P.ClienteId = C.ClienteId
WHERE C.Ativo = 0;

```

---

## 📊 Parte 4 – Agregações com GROUP BY, SUM, AVG, COUNT, HAVING

### SUM – Soma valores

```sql
SELECT ClienteId, SUM(Valor) AS TotalGasto
FROM TB_PEDIDO
GROUP BY ClienteId;

```

### AVG – Média de valores

```sql
SELECT ClienteId, AVG(Valor) AS MediaGasto
FROM TB_PEDIDO
GROUP BY ClienteId;

```

### COUNT – Contar quantos registros

```sql
SELECT ClienteId, COUNT(*) AS TotalPedidos
FROM TB_PEDIDO
GROUP BY ClienteId;

```

### MIN / MAX – Valor mínimo ou máximo

```sql
SELECT ClienteId, MIN(Valor) AS MenorPedido, MAX(Valor) AS MaiorPedido
FROM TB_PEDIDO
GROUP BY ClienteId;

```

### HAVING – Filtro para agregações (como WHERE, mas após o GROUP BY)

```sql
SELECT ClienteId, SUM(Valor) AS TotalGasto
FROM TB_PEDIDO
GROUP BY ClienteId
HAVING SUM(Valor) > 500;

```

- WHERE filtra linhas antes do GROUP BY
- HAVING filtra resultados agregados

---

## 🧪 Desafios e Exercícios Práticos

1. Liste todos os clientes com nome, total de pedidos e valor médio por pedido.
2. Mostre os 3 clientes que mais gastaram em pedidos.
3. Liste todos os pedidos realizados em 2024 e o total mensal (usar `GROUP BY MONTH(DataPedido)`).
4. Mostre os produtos com estoque abaixo da média (usar subquery com AVG).
5. Liste os clientes que não fizeram nenhum pedido (usar LEFT JOIN e WHERE P.PedidoId IS NULL).

---

Esse conteúdo é construído para te levar da base até a aplicação prática, com foco no ambiente corporativo e uso profissional.

## 🔗 Parte 3 – JOINs e Relacionamentos (Aprofundado)

### 🔍 O que é JOIN?

JOIN é o mecanismo que o SQL oferece para **ligar dados entre tabelas** usando colunas que se relacionam. Ele é fundamental em bancos relacionais porque permite que você trabalhe com dados distribuídos em tabelas diferentes de forma eficiente.

---

### 🧠 INNER JOIN – Retorna apenas os dados que têm correspondência nas duas tabelas

```
SELECT C.Nome, P.Valor, P.DataPedido
FROM TB_CLIENTE C
INNER JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;
```

**Explicação:**

- Só serão retornados clientes que fizeram ao menos um pedido.
- Se o cliente existir, mas não tiver pedido, ele será ignorado.

**Visualização lógica:**

```
[TABELA CLIENTE] + [TABELA PEDIDO] => Só onde existe correspondência de ClienteId
```

---

### 🧠 LEFT JOIN – Traz todos os dados da tabela da esquerda

```
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
LEFT JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;
```

**Explicação:**

- Todos os clientes aparecerão.
- Se algum cliente não tiver pedido, os campos de pedido virão como `NULL`.
- Muito usado para encontrar “quem está faltando”.

---

### 🧠 RIGHT JOIN – O inverso do LEFT JOIN

```
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
RIGHT JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;
```

**Explicação:**

- Todos os pedidos serão retornados.
- Clientes que não existem mais mas ainda têm registros na tabela de pedidos (casos de inconsistência) ainda aparecerão.

---

### 🧠 FULL OUTER JOIN – Junta tudo das duas tabelas

```
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
FULL OUTER JOIN TB_PEDIDO P ON C.ClienteId = P.ClienteId;
```

**Explicação:**

- Mostra todos os clientes e todos os pedidos, mesmo que não tenham correspondência.
- Ideal para auditorias.

---

### 🧠 CROSS JOIN – Produto cartesiano

```
SELECT C.Nome, P.Valor
FROM TB_CLIENTE C
CROSS JOIN TB_PEDIDO P;
```

**Explicação:**

- Junta cada linha de uma tabela com todas da outra.
- Pode explodir a quantidade de registros. Use com muito cuidado.

---

### 🔄 UPDATE com JOIN

```
UPDATE P
SET P.Valor = P.Valor * 0.9
FROM TB_PEDIDO P
INNER JOIN TB_CLIENTE C ON P.ClienteId = C.ClienteId
WHERE C.Nome LIKE '%João%';
```

**Explicação:**

- Aplica 10% de desconto nos pedidos feitos por clientes cujo nome contém “João”.
- `FROM` com `JOIN` é necessário no SQL Server para fazer UPDATE com dados de outra tabela.

---

### 🗑️ DELETE com JOIN

```
DELETE P
FROM TB_PEDIDO P
INNER JOIN TB_CLIENTE C ON P.ClienteId = C.ClienteId
WHERE C.Ativo = 0;
```

**Explicação:**

- Remove todos os pedidos de clientes inativos.
- Ideal para manutenções e limpezas.

---

## 📊 Parte 4 – Funções de Agregação e Agrupamento (Aprofundado)

### 🧠 Por que usar GROUP BY?

O `GROUP BY` permite agrupar dados que têm algo em comum e aplicar funções como `SUM`, `AVG`, `COUNT`, etc.

Ele é essencial para gerar relatórios, KPIs e dashboards em SQL.

---

### 🔢 SUM – Somar valores

```
SELECT ClienteId, SUM(Valor) AS TotalGasto
FROM TB_PEDIDO
GROUP BY ClienteId;
```

**Explicação:**

- Agrupa por cliente.
- Soma o valor total gasto por cada um.

---

### 🎯 AVG – Média dos valores

```
SELECT ClienteId, AVG(Valor) AS MediaPedidos
FROM TB_PEDIDO
GROUP BY ClienteId;
```

**Explicação:**

- Média dos valores por cliente.

---

### 🔢 COUNT – Contar registros

```
SELECT ClienteId, COUNT(*) AS QtdePedidos
FROM TB_PEDIDO
GROUP BY ClienteId;
```

**Explicação:**

- Quantos pedidos cada cliente fez.

---

### 📉 MIN / MAX – Menor e maior valores

```
SELECT ClienteId, MIN(Valor) AS PedidoMaisBarato, MAX(Valor) AS PedidoMaisCaro
FROM TB_PEDIDO
GROUP BY ClienteId;
```

---

### 🔍 HAVING – Filtro após agrupamento

```
SELECT ClienteId, SUM(Valor) AS TotalGasto
FROM TB_PEDIDO
GROUP BY ClienteId
HAVING SUM(Valor) > 500;
```

**Explicação:**

- O `WHERE` funciona antes do agrupamento.
- O `HAVING` filtra depois da agregação.

---

###
