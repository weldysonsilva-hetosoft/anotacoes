# 🗂️ Guia Prático de Conversão de Dados - Hetosoft

📄 Baseado no arquivo original do GitHub:  
[Documentacao Basica.md](https://github.com/hetosoft/ProjetosSol.NET/blob/develop/Documentacao/Conversao/Documentacao%20Basica.md)

---

## ✅ Objetivo do Documento

Orientar sobre os processos básicos de conversão de dados de bases externas para os sistemas da Hetosoft, garantindo integridade, consistência e performance.

---

## 📌 Pontos-chave Resumidos

### 1. Entendimento Prévio
- Conhecer as tabelas do cliente e suas relações.
- Validar as informações com o analista responsável.
- Verificar volume e estrutura dos dados (tipo, tamanho, colunas).

### 2. Criação das Tabelas Temporárias
- Utilizar `CREATE TABLE` para espelhar a estrutura da base de origem.
- Tipos de dados devem ser compatíveis com os do sistema.
- Criar índices, se necessário, para melhorar performance em `JOIN` e `SELECT`.

### 3. Importação dos Dados
- Ferramentas possíveis:  
  - `bcp`  
  - SQL Server Import and Export Wizard  
  - Scripts SQL

- Verificar se há dados nulos ou inconsistentes.
- Rodar `SELECT COUNT(*)` após a importação para conferência.

### 4. Tratamento e Mapeamento
Utilizar funções SQL comuns:

```sql
CAST(), CONVERT(), ISNULL(), COALESCE()
JOINs: INNER JOIN, LEFT JOIN
Funções: GROUP BY, HAVING, SUM(), COUNT()
