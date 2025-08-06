# 📊 Mapeamento das Funções SQL Mais Utilizadas nos Formulários

**Projeto:** Sol.NET - Sistema de Conversão  
**Repositório:** hetosoft/ProjetosSol.NET  
**Pasta Analisada:** Sol.NET_Conversao  
**Data da Análise:** 2025-08-06  
**Analisado por:** wesProg23  

---

## 📋 **Resumo Executivo**

Este documento apresenta um mapeamento detalhado das funções SQL mais utilizadas nos formulários de conversão do sistema Sol.NET. A análise foi realizada através de busca semântica e lexical no código-fonte, identificando padrões de uso e exemplos práticos de implementação.

---

## 🎯 **Principais Funções SQL Identificadas**

### 1. **IIF (Immediate If)**

**Descrição:** Função condicional simples que retorna um valor baseado em uma condição booleana. É amplamente utilizada para conversões rápidas de flags e status.

**Sintaxe:**
```sql
IIF(condição, valor_se_verdadeiro, valor_se_falso)
```

**Casos de Uso Principais:**
- Conversão de flags S/N para 1/0
- Verificação de status ativos/inativos
- Seleção condicional de campos
- Tratamento de tipos de pessoa

**Exemplos Práticos:**

```sql
-- Conversão de status ativo/inativo
IIF(ativo = 'S', 0, 1)

-- Verificação de cliente
IIF(P.PES_CLIENTE = 'S', 1, 0)

-- Seleção de email para NFe
IIF(PC.PCO_EMAIL_NFE = 'S', PC.PCO_CONTATO, '')

-- Determinação de tipo de documento
IIF(COALESCE(P.CPF, '') = '', P.CNPJ, P.CPF)

-- Verificação de fornecedor/cliente
IIF(C.USARFORNECEDOR = 1, 'FORNECEDOR', 'CLIENTE')

-- Limite de crédito
IIF(VALOR_LIMITE > 0, 1, 0)

-- Verificação de sexo
IIF(Sexo LIKE '%F%', 2, 1)

-- Status de cancelamento
IIF(C.CANCELADA = 1, 2, 1)
```

---

### 2. **CASE WHEN**

**Descrição:** Estrutura condicional complexa que permite múltiplas verificações em cascata. Utilizada quando há necessidade de avaliar várias condições sequenciais.

**Sintaxe:**
```sql
CASE 
    WHEN condição1 THEN valor1
    WHEN condição2 THEN valor2
    WHEN condição3 THEN valor3
    ELSE valor_padrao
END
```

**Casos de Uso Principais:**
- Mapeamento de códigos complexos
- Conversão de estados civis
- Classificação de tipos de contribuinte
- Transformação de status múltiplos

**Exemplos Práticos:**

```sql
-- Conversão de status ativo/inativo
CASE WHEN ativo = 'S' THEN 0 ELSE 1 END

-- Verificação de estoque principal
CASE WHEN estoque_geral = 'S' THEN 1 ELSE 0 END

-- Estados civis complexos
CASE 
    WHEN [Estado Civil] = 'DIVORCIADO(A)' THEN 4
    WHEN [Estado Civil] = 'SOLTEIRO(A)' THEN 2
    WHEN [Estado Civil] = 'CASADO(A)' THEN 1
    WHEN [Estado Civil] = 'UNIAO ESTAVEL' THEN 1
    ELSE -1
END

-- Tipos de contribuinte
CASE 
    WHEN P1.PD_TIPO_CONTRIBUINTE = 0 THEN 1
    WHEN P1.PD_TIPO_CONTRIBUINTE = 1 THEN 2
    WHEN P1.PD_TIPO_CONTRIBUINTE = 2 THEN 9
    ELSE -1
END

-- Fracionamento de unidade
CASE WHEN fracionada = 'S' THEN 1 ELSE 0 END
```

---

### 3. **COALESCE**

**Descrição:** Retorna o primeiro valor não-nulo de uma lista de expressões. Essencial para tratamento de valores padrão e campos opcionais.

**Sintaxe:**
```sql
COALESCE(valor1, valor2, valor3, ...)
```

**Casos de Uso Principais:**
- Definição de valores padrão
- Tratamento de campos nulos
- Seleção do primeiro valor válido
- Verificação de existência de dados

**Exemplos Práticos:**

```sql
-- Valor padrão para alíquotas
COALESCE(II.ICMS, 0)

-- Redução de base de cálculo
100 - COALESCE(II.REDUCAO_ICMS, 0)

-- Seleção do primeiro nome disponível
COALESCE(NULLIF(TRIM(NOME), ''), NULLIF(TRIM(FANTASIA), ''))

-- Verificação de NCM não vazio
COALESCE(I.NCM, '') <> ''

-- Primeiro telefone disponível
COALESCE(FONE, FONE2, FONE3)

-- Número de parcela padrão
COALESCE(CP.NUM_PARCELA, 1)

-- Tratamento de CEST
COALESCE(CEST, '')
```

---

### 4. **CAST**

**Descrição:** Converte um tipo de dado para outro tipo específico. Fundamental para adequação de tipos entre diferentes sistemas de banco de dados.

**Sintaxe:**
```sql
CAST(expressão AS tipo_destino)
```

**Casos de Uso Principais:**
- Conversão de datas e horários
- Adequação de tipos numéricos
- Transformação para VARCHAR
- Separação de data e hora

**Exemplos Práticos:**

```sql
-- Conversão para data
CAST(data_hora_atualizacao AS DATE)

-- Conversão para hora
CAST(data_hora_atualizacao AS TIME)

-- Conversão para texto com tamanho específico
CAST(Observacao AS VARCHAR(2048))

-- Criação de chave composta
CAST(COD_CLJ AS VARCHAR(5)) + 'JURIDICO'

-- Conversão de observações
CAST(OB_OBSERVACAO AS VARCHAR(256))

-- Data de nascimento
CAST([Data Nascimento] AS DATE)

-- Telefone como texto
CAST(Celular AS VARCHAR(15))
```

---

### 5. **NULLIF**

**Descrição:** Retorna NULL se duas expressões são iguais, caso contrário retorna a primeira expressão. Utilizada para "limpar" dados inválidos.

**Sintaxe:**
```sql
NULLIF(expressão1, expressão2)
```

**Casos de Uso Principais:**
- Limpeza de strings vazias
- Tratamento de valores "ISENTO"
- Remoção de dados inválidos
- Normalização de campos

**Exemplos Práticos:**

```sql
-- Limpar strings vazias após trim
NULLIF(TRIM(NOME), '')

-- Tratar inscrição estadual isenta
NULLIF(IE, 'ISENTO')

-- Limpar fantasia vazia
NULLIF(TRIM(FANTASIA), '')

-- Tratamento de RG/IE
NULLIF(IE_RG, '')
```

---

### 6. **Operadores de Concatenação**

**Descrição:** União de strings ou valores para formar chaves compostas ou campos concatenados.

**Sintaxes:**
```sql
-- Firebird/PostgreSQL
campo1 || campo2

-- SQL Server
campo1 + campo2

-- Função CONCAT (universal)
CONCAT(campo1, campo2)
```

**Casos de Uso Principais:**
- Criação de chaves compostas
- Formação de códigos únicos
- Concatenação de NCM com tributação
- União de identificadores

**Exemplos Práticos:**

```sql
-- Chave composta grupo/subgrupo
S.cod_grupos || '#' || s.codigo

-- NCM completa com tributação
NCM || CSOSN || CSTICMS || CSTE || COALESCE(CEST, '') || ALIQ_ICM

-- Chave de pessoa jurídica
CAST(COD_CLJ AS VARCHAR(5)) + 'JURIDICO'

-- Concatenação de PIS
CONCAT(['PRO_CST_PIS_ENTRADA', 'PRO_CST_PIS_SAIDA'], '#')
```

---

### 7. **CONTAINING (Firebird)**

**Descrição:** Operador específico do Firebird que verifica se uma string contém um substring específico.

**Sintaxe:**
```sql
campo CONTAINING 'substring'
```

**Casos de Uso Principais:**
- Verificação de tipos múltiplos
- Busca de padrões em campos
- Classificação baseada em conteúdo

**Exemplos Práticos:**

```sql
-- Verificação de tipo cliente
IIF(P1.TIPO CONTAINING '0', 1, 0)

-- Verificação de tipo fornecedor
IIF(P1.TIPO CONTAINING '1', 1, 0)

-- Verificação de tipo transportador
IIF(P1.TIPO CONTAINING '2', 1, 0)

-- Verificação de tipo funcionário
IIF(P1.TIPO CONTAINING '3', 1, 0)
```

---

## 📊 **Padrões de Uso Identificados**

### 1. **Conversão de Flags Booleanas**
```sql
-- Padrão mais comum
IIF(campo = 'S', 1, 0)
CASE WHEN ativo = 'S' THEN 0 ELSE 1 END
```

### 2. **Tratamento de Campos Opcionais**
```sql
-- Valores padrão
COALESCE(campo_opcional, valor_padrao)

-- Limpeza de dados
NULLIF(TRIM(campo), '')
```

### 3. **Adequação de Tipos**
```sql
-- Conversões necessárias
CAST(campo AS tipo_destino)

-- Separação de data/hora
CAST(datetime_field AS DATE)
CAST(datetime_field AS TIME)
```

### 4. **Criação de Chaves Compostas**
```sql
-- Concatenação de identificadores
campo1 || '#' || campo2
CAST(id AS VARCHAR) + 'SUFIXO'
```

### 5. **Seleção Condicional de Dados**
```sql
-- Primeira opção válida
COALESCE(NULLIF(TRIM(campo1), ''), NULLIF(TRIM(campo2), ''))

-- Telefones em ordem de prioridade
COALESCE(FONE, FONE2, FONE3)
```

---

## 🎯 **Recomendações de Uso**

### **Para IIF:**
- Use para condições simples e conversões diretas
- Ideal para flags booleanas S/N → 1/0
- Mantenha a lógica simples e legível

### **Para CASE WHEN:**
- Use para lógicas complexas com múltiplas condições
- Ideal para mapeamentos de códigos
- Sempre inclua ELSE para casos não previstos

### **Para COALESCE:**
- Use para definir valores padrão
- Combine com NULLIF para limpeza de dados
- Essencial em migrações entre sistemas

### **Para CAST:**
- Necessário para compatibilidade entre SGBDs
- Use tipos explícitos e tamanhos adequados
- Teste conversões com dados reais

### **Para NULLIF:**
- Use para limpar dados inconsistentes
- Combine com TRIM para strings
- Trate valores especiais como 'ISENTO'

---

## 📈 **Estatísticas de Uso**

Com base na análise do código:

- **IIF:** Função mais utilizada (40+ ocorrências)
- **CASE WHEN:** Segunda mais comum (15+ ocorrências)
- **COALESCE:** Amplamente usado para tratamento de nulos (20+ ocorrências)
- **CAST:** Essencial para conversões de tipo (15+ ocorrências)
- **NULLIF:** Usado para limpeza de dados (10+ ocorrências)

---

## ⚠️ **Considerações Importantes**

1. **Compatibilidade:** Algumas funções são específicas de SGBDs
2. **Performance:** IIF é mais rápido que CASE para condições simples
3. **Manutenibilidade:** CASE WHEN é mais legível para lógicas complexas
4. **Tratamento de Nulos:** Sempre considere valores NULL em condições
5. **Testes:** Valide conversões com dados reais antes da produção

---

## 🔗 **Referências**

- [Repositório Sol.NET](https://github.com/hetosoft/ProjetosSol.NET)
- [Pasta de Conversão](https://github.com/hetosoft/ProjetosSol.NET/tree/develop/Sol.NET_Conversao)
- [Busca por Funções SQL](https://github.com/hetosoft/ProjetosSol.NET/search?q=IIF+CASE+COALESCE+CAST&type=code)

---

**Documento gerado em:** 2025-08-06 18:03:50 UTC  
**Versão:** 1.0  
**Autor:** wesProg23
