# 🔧 Procedure em Delphi

Uma **procedure** em Delphi é um bloco de código que executa uma tarefa, mas **não retorna valor diretamente**. Procedures são ideais para operações de comando, lógica de fluxo e manipulação de dados quando não há necessidade de retornar um resultado específico, apenas realizar uma ação.

---

## 🧩 Sintaxe Básica

```delphi
procedure NomeDaProcedure(Parametro1: Tipo1; Parametro2: Tipo2);
begin
  // Corpo da procedure
end;
```

### 📌 Explicação da Sintaxe

- **procedure**: palavra-chave que inicia a declaração.
- **NomeDaProcedure**: identificador da procedure.
- **Parâmetros**: declarados entre parênteses, cada um com nome e tipo separados por `:`.
- O corpo da procedure é delimitado por `begin ... end`.
- Procedures não possuem tipo de retorno.

---

## ✅ Exemplo com Múltiplos Parâmetros

```delphi
procedure AtualizarEstoque(CodigoProduto: Integer; Quantidade: Integer; Motivo: String);
begin
  // Atualiza o estoque do produto informado
  // Pode registrar o motivo em log, disparar eventos, etc.
end;
```

---

## 🧪 Exemplos Usados no Sol.NET

Procedures são amplamente utilizadas tanto para lógica de negócio quanto para integração com o banco de dados, especialmente em eventos de UI e execução de tarefas no Sol.NET.

### Exemplo 1: Procedure para disparo de rotina de conversão

```delphi
procedure TFrmMeuCliente.BotaoMinhaEntidade;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
    .SetTabelaConversao(TTabelaPessoa.Create(Pessoa), 'MINHA_TABELA_ORIGEM MT')
    .AddPrimaryKey('MT.CODIGO')
    .AddCampo('NOME', 'MT.NOME')
    .AddCampo('EMAIL', 'MT.EMAIL')
    .AddCampo('CPF', 'MT.CPF', nil, '', '', [TFuncoes.SoNumeros])
    .AddCampo('INATIVO', 'CASE WHEN MT.ATIVO = ''S'' THEN 0 ELSE 1 END')
    .AddWhere('MT.DATA_CADASTRO >= ' + QuotedStr('2020-01-01'))
    .AddJoin('LEFT JOIN ENDERECOS E ON E.CODIGO_PESSOA = MT.CODIGO')
    .Build;

  // Executar conversão usando método herdado
  ConversaoPessoas(ParametrosConversao);
end;
```

### Exemplo 2: Procedure tradicional de manipulação de dados

```delphi
procedure ProcessarPedidos(PedidoID: Integer);
begin
  // Lógica para processar um pedido específico
end;
```

---

## ℹ️ Dicas e Boas Práticas

- Use nomes descritivos para procedures e parâmetros.
- Segmente procedures grandes em procedures menores para facilitar manutenção.
- Utilize variáveis inline seguindo o padrão do projeto:  
  ```delphi
  var MinhaVariavel: Integer := 0;
  ```
- Utilize procedures para comandos, lógicas de evento e manipulações diretas de dados, mantendo funções para lógicas que precisam retornar valores.

---

## 📚 Referência Rápida

- Procedures são essenciais para a organização de código em Delphi.
- Use procedures para encapsular ações e comandos repetitivos.
- Documente procedures complexas com comentários claros sobre sua responsabilidade.

---






# 🔧 Funções (Function) em Delphi

Uma **function** em Delphi é um bloco de código reutilizável que executa uma tarefa específica e retorna um valor. Funções são amplamente utilizadas no projeto Sol.NET para encapsular lógicas de negócio, cálculos e ações de validação, tornando o código mais organizado e reutilizável.

---

## 🧩 Sintaxe Básica

```delphi
function NomeDaFunction(Parametro1: Tipo1; Parametro2: Tipo2): TipoDeRetorno;
begin
  // Corpo da função
end;
```

### 📌 Explicação da Sintaxe

- **function**: palavra-chave que inicia a declaração.
- **NomeDaFunction**: identificador único da função.
- **Parâmetros**: declarados entre parênteses, com nome e tipo separados por `:`.
- **Tipo de Retorno**: especificado após o parêntese de fechamento.
- **Corpo da função**: delimitado por `begin ... end`.
- **Result**: variável implícita usada para atribuir o valor de retorno.

---

## ✅ Exemplo com Múltiplos Parâmetros

```delphi
function CalcularDesconto(Preco: Double; Quantidade: Integer; Categoria: String; ClienteVIP: Boolean): Double;
begin
  if ClienteVIP then
    Result := Preco * Quantidade * 0.85  // 15% de desconto para VIPs
  else if Categoria = 'Promocao' then
    Result := Preco * Quantidade * 0.90  // 10% de desconto para produtos em promoção
  else
    Result := Preco * Quantidade;        // sem desconto
end;
```

---

## 🧪 Exemplo Real em Sol.NET

No contexto do Sol.NET, funções são frequentemente utilizadas para callbacks, validações e montagem de dados para processos de conversão. Por exemplo:

```delphi
function MontagemListaProdSimilares(cdsVinculos: TClientDataSet): TCallbackConversao;
begin
  // Implementação específica para montagem de lista de produtos similares
end;
```

Neste exemplo, a função recebe um `TClientDataSet` como parâmetro e retorna um objeto do tipo `TCallbackConversao`, que é utilizado em fluxos de conversão de dados no Sol.NET.

---

## ℹ️ Dicas e Boas Práticas

- Utilize nomes descritivos para funções e parâmetros.
- Prefira funções pequenas, com responsabilidade única.
- Documente funções complexas utilizando comentários ou blocos de documentação.
- Use o padrão de declaração de variáveis inline conforme a convenção do Sol.NET:
  ```delphi
  var MinhaVariavel: Integer := 0;
  ```
---

# Function vs Procedure - Regra Prática

## **PROCEDURE** 🔨

**Faz uma ação, não retorna nada**

```pascal

`procedure MostrarMensagem(texto: string);
begin
  ShowMessage(texto);  *// Só executa, não devolve nada*
end;

*// Uso:*
MostrarMensagem('Olá');  *// Executa e pronto*`
````


## **FUNCTION** 📦

**Faz uma ação E retorna um resultado**

```pascal

`function Somar(a, b: Integer): Integer;
begin
  Result := a + b;  *// Retorna o resultado*
end;

*// Uso:*
x := Somar(5, 3);  *// x recebe 8*
if Somar(2, 2) = 4 then  *// Usa o retorno para decidir algo*
  ShowMessage('Correto!');`
```


## **Quando usar cada um?**

```
SituaçãoUseSó executar algo (salvar, mostrar, deletar)ProcedurePrecisa do resultado para decidir/usar depoisFunctionCalcular, validar, verificar algoFunctionOperação sem retorno relevanteProcedure
```



## **Exemplo prático:**

```pascal

`*// PROCEDURE - só exporta*
procedure ExportarCSV(dados: TDataSet);
begin
  *// exporta e pronto*
end;

*// FUNCTION - exporta E diz se deu certo*
function ExportarCSV(dados: TDataSet): Boolean;
begin
  *// exporta*
  Result := True;  *// retorna sucesso/falha*
end;

*// Uso da function:*
if ExportarCSV(Query) then
  ShowMessage('Deu certo!')
else
  ShowMessage('Erro!');
```

**Resumão:** Function = retorna algo útil | Procedure = só faz a ação 🎯

--- 

## Callback em Delphi (Padrão Sol.NET)

No contexto do Sol.NET, um **callback** é um procedimento ou função (incluindo métodos anônimos) que pode ser registrado para ser executado em momentos específicos do ciclo de processamento, como validação, pré-processamento, pós-processamento ou ajuste de dados. O uso de callbacks traz flexibilidade e desacoplamento, permitindo customizar comportamentos sem alterar a lógica central.

---

### O que é Callback?

Callback é um conceito em que uma rotina (procedure/função) é passada como parâmetro ou registrada para ser chamada automaticamente em determinado ponto da execução. Em Delphi, pode ser implementado por métodos anônimos, ponteiros de método, eventos ou objetos dedicados.

---

### Uso de Callback na Conversão de Dados (Sol.NET_Conversao)

No módulo **SolNET_Conversao**, callbacks são encapsulados na classe `TCallbackConversao` e associados a um momento do ciclo de conversão por meio do tipo `TMomentoConversao`. Eles são usados para validações, cálculos extras, ajustes, logs, etc.

**Exemplo prático:**

```delphi
function ValidarDadosBasicos: TCallbackConversao;
begin
  Result := TCallbackConversao.Create(
    procedure(Conversao: TConversao)
    begin
      // Regras de validação customizadas
      if not ValidarCPF(Conversao.Origem.FieldByName('CPF').AsString) then
        raise Exception.Create('CPF inválido!');
    end,
    TMomentoConversao.Validacao
  );
end;

// Registro do callback:
ParametrosConversao.ListaCallbacks.Add(ValidarDadosBasicos);
```

**Momentos disponíveis para execução (TMomentoConversao):**
- `PreProc`: Antes de iniciar o processamento
- `Validacao`: Durante validação de dados
- `PreExec`: Antes de executar cada registro
- `PosExec`: Após executar cada registro
- `PosProc`: Após finalizar o processamento

---

### Uso de Callback em Integrações (Framework/Integracoes)

No **Framework de Integrações** (pasta Framework/Integracoes), o conceito de callback aparece em métodos que podem ser passados como referência ou que são chamados em resposta a eventos de integração, como logs, auditorias, notificações de status, etc.

**Exemplo típico:**

```delphi
procedure ProcessarIntegracao;
begin
  IniciarTransacao;
  try
    // Processamento principal...
    LogAdd('Processo finalizado'); // Callback para registro de log
    CommitTransacao;
  except
    RollbackTransacao;
    raise;
  end;
end;
```
Ou, para notificações:

```delphi
Integracao.Handshake(siProcessoExecutando, 'MinhaEmpresa', 'ProcessoX', 'Resumo da integração');
```

---

### Boas Práticas para Callback no Sol.NET

- Use callbacks para centralizar validações e customizações sem alterar o fluxo principal.
- Prefira métodos anônimos (`procedure of object`) quando possível, para maior flexibilidade.
- Sempre registre callbacks no momento correto do ciclo de conversão ou integração.
- Documente a intenção e o momento de execução do callback.
- Utilize callbacks para logs, validação de dados, auditoria, notificações e pós-processamento.

---

### Resumo

- **Callback** é um mecanismo essencial tanto em conversão de dados quanto em integrações no Sol.NET.
- Traga flexibilidade, reutilização e desacoplamento para o código.
- Utilize os tipos e classes padrões do projeto: `TCallbackConversao`, `TMomentoConversao`, além de métodos de log/auditoria nas integrações.

---

> Consulte também:  
> [Documentação Básica SolNET_Conversao](Documentacao/Conversao/Documentacao%20Basica.md)  
> [Framework de Integrações](Documentacao/Integracoes/Framework%20Integracoes.md)
