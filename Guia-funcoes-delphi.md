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

> **Referência:**  
> Consulte o [Guia de Desenvolvimento Sol.NET](../Documentacao/Conversao/Documentacao%20Basica.md) para exemplos práticos de uso de funções e padrões de codificação.
