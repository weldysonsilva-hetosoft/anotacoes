# Convertendo uma nova entidade

1- Indentificar a tabela referente a entidade no DB origem

2- Mapear campos

2.1- Criar a consulta listando os campos necessarios

2.2- Apelidar os campos com os nomes Sol.net

3- Criar parametro conversão com os campos mapeados 

4- Fazer a chamada da conversão através do metodo Conversao{Nome da entidade}

## **📋 Dicas para um Guia de Mapeamento de Campos**

### **1. Defina a Tabela Destino e Origem Claramente**

- Sempre inicie o mapeamento informando qual tabela será o destino no Sol.NET e qual a origem dos dados.
- Utilize nomes e aliases consistentes:Exemplo: `'CLIENTES C'` para origem, `TTabelaPessoa.Create(Pessoa)` para destino.

### **2. Mapeie as Chaves Primárias Primeiro**

- Identifique e mapeie os campos de chave primária tanto na origem quanto no destino.
- Use `.AddPrimaryKey('C.CODIGO')` para garantir integridade referencial.

### **3. Separe Campos Reais de Virtuais**

- **Campos reais:** correspondem a colunas físicas da tabela destino.
- **Campos virtuais:** são aliases, expressões calculadas ou campos auxiliares para suporte a transformações.
- Use `.AddCampo(...)` para reais e `.AddCampoVirtual(...)` para virtuais.

### **4. Utilize Funções de Transformação Sempre Que Necessário**

- Padronize limpezas e normalizações com `TFuncoes` no mapeamento:
    - Exemplo: `[TFuncoes.SoNumeros]` para CPF/CNPJ
    - Evite funções manuais inline quando já existe função padronizada.

### **5. Documente Lookups e BuscarDinamico**

- Se o campo mapeado depende de consulta em tabela auxiliar (de/para), explicite isso.
- Exemplo: campos que usam `TabelaBuscar` ou `BuscarDinamico` para resolver FK ou valores referenciais.

### **6. Utilize Expressões SQL para Regras de Negócio**

- Use expressões para transformar valores conforme regras:
    - Exemplo: `'CASE WHEN ATIVO = ''S'' THEN 0 ELSE 1 END'` para campo INATIVO.
- Evite lógica de transformação no código Delphi se puder ser resolvida via SQL.

### **7. Padronize a Nomenclatura dos Campos**

- Siga convenções do Sol.NET:
    - PascalCase para nomes
    - Prefixos claros (ex: ID_, COD_, NOME_)
    - Evite abreviações obscuras

### **8. Valide Campos Obrigatórios**

- Sempre destaque e documente campos que são obrigatórios para o funcionamento do sistema.
- Use callbacks para validação extra quando necessário.

### **9. Referencie Conversões Compostas (Sub-conversões)**

- Se existirem relacionamentos 1:N (endereços, contatos), documente como o campo FK será mapeado e vinculado à entidade principal.

### **10. Inclua Exemplos Sintéticos**

- Insira exemplos reais ou fictícios de mapeamento para facilitar a compreensão do padrão esperado.

### **11. Documente Tratamento de Erros e Anomalias**

- Oriente sobre o uso de callbacks de validação para detectar dados inválidos ou duplicados.
- Explique como registrar erros e ajustar o mapeamento conforme necessário.

### **12. Considere Compatibilidade entre SGBDs**

- Sempre que usar funções SQL, certifique-se de que a sintaxe atende tanto Firebird quanto SQL Server.
- Utilize helpers como `SQLOrigem.CONCAT` para garantir portabilidade.

---

## **Exemplos Práticos**

Pascal

`.AddCampo('CPF', 'C.CPF', nil, '', '', [TFuncoes.SoNumeros]) // Limpa caracteres não-numéricos
.AddCampo('INATIVO', 'CASE WHEN C.ATIVO = ''S'' THEN 0 ELSE 1 END') // Expressão SQL
.AddCampoVirtual('NOME_COMPLETO', 'C.NOME || '' '' || C.SOBRENOME') // Campo virtual`

---

## **Checklist de Mapeamento**

- [ ]  Tabela destino definida
- [ ]  Tabela origem e alias definidos
- [ ]  Chave primária mapeada
- [ ]  Todos os campos obrigatórios mapeados
- [ ]  Funções de transformação aplicadas
- [ ]  Campos virtuais documentados
- [ ]  Lookups e tabelas auxiliares referenciadas
- [ ]  Expressões SQL compatíveis
- [ ]  Callback de validação incluído (se necessário)
- [ ]  Exemplo de uso mostrado
