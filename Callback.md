# ⚡ Callbacks no Sol.NET — Guia Completo (para iniciantes)

Esta documentação explica de forma prática e detalhada como funcionam os **callbacks** no contexto do Sol.NET — tanto na conversão de dados (SolNET_Conversao) quanto no Framework de Integrações. O objetivo é que desenvolvedores iniciantes entendam a lógica, como criar, registrar, executar e testar callbacks, além de boas práticas e armadilhas comuns.

Sumário
- O que é um callback?
- Tipos de callbacks em Delphi (no projeto)
- Callbacks na conversão (SolNET_Conversao)
  - Estrutura (TCallbackConversao / TMomentoConversao)
  - Exemplo real com ConversaoBuilder
  - Exemplo tradicional (sem Builder)
  - Como os callbacks são executados no fluxo
- Callbacks em integrações (Framework/Integracoes)
  - Exemplos: logging, auditoria, resumo de integração
  - Uso com IIntegracaoPlataforma e TResumoIntegracao
- Boas práticas e recomendações
- Tratamento de erros e transações
- Testes e debugging
- Checklist rápido

---

## O que é um callback?

Um **callback** é uma rotina (procedure/função/método anônimo/objeto) que você passa ou registra para ser executada automaticamente em um ponto específico do fluxo de execução. Ele permite personalizar comportamento sem alterar a lógica central.

No Sol.NET, callbacks tornam conversões e integrações configuráveis e extensíveis: validações, ajustes, logs, transformações extras, etc.

---

## Tipos de callbacks em Delphi (usados no projeto)

- Procedures/métodos tradicionais (procedure of object)
- Métodos anônimos (anonymous procedures)
- Objetos que encapsulam lógica (ex.: `TCallbackConversao`)
- Funções que retornam um wrapper (`function ValidarCPF: TCallbackConversao;`)

No Sol.NET, a camada de conversão usa `TCallbackConversao` para empacotar um procedimento com um momento (`TMomentoConversao`) — veja a seguir.

---

## Callbacks na conversão (SolNET_Conversao)

### Estrutura principal

- TCallbackConversao: classe que encapsula
  - o procedimento a ser executado (normalmente `procedure(Conversao: TConversao)` ou similar)
  - o momento em que será executado (`TMomentoConversao`)
- TMomentoConversao: enum que define quando o callback será acionado:
  - PreProc — antes de iniciar o processamento do bloco
  - PreExec — antes de processar cada registro
  - Validacao — momento de validações (pode ser antes ou durante)
  - PosExec — após processar cada registro
  - PosProc — após finalizar o processamento do bloco

### Exemplo simples com método que retorna TCallbackConversao

```delphi
function ValidarCPF: TCallbackConversao;
begin
  Result := TCallbackConversao.Create(
    procedure(Conversao: TConversao)
    begin
      var CPF: string := Conversao.Parametros.cdsOrigem.FieldByName('CPF').AsString;
      if not Geral.ValidarCPF(CPF) then
        raise EFailValidacao.CreateFmt('CPF inválido: %s', [CPF]);
    end,
    TMomentoConversao.Validacao
  );
end;
```

Neste exemplo:
- O callback é um método anônimo que acessa o dataset de origem (`cdsOrigem`).
- Se a validação falhar, levanta-se uma exceção específica (isso será tratada pelo fluxo de conversão).

### Como registrar callbacks (com ConversaoBuilder)

Exemplo usando o padrão Builder (recomendado):

```delphi
var
  Parametros: TParametrosConversao;
begin
  Parametros := TConversaoBuilder.Create
    .SetTabelaConversao(TTabelaPessoa.Create(Pessoa), 'CLIENTES C')
    .AddPrimaryKey('C.CODIGO')
    .AddCampo('NOME', 'C.NOME')
    .AddCampo('CPF', 'C.CPF', nil, '', [TFuncoes.SoNumeros])
    .Build;

  // Registrar callbacks
  Parametros.ListaCallbacks.Add(ValidarCPF);
  Parametros.ListaCallbacks.Add(Procedure
    begin
      // Callback anônimo de exemplo (PosExec)
    end, TMomentoConversao.PosExec);

  // Executar conversão (ex.: ConversaoPessoas(Parametros))
end;
```

Observação: a API para adicionar depende da implementação; normalmente existe um método para adicionar instâncias de `TCallbackConversao` ou procedimentos com momento.

### Exemplo tradicional (sem Builder)

```delphi
var
  Parametros: TParametrosConversao;
  ParamSQL: TParametroSQL;
begin
  Parametros := TParametrosConversao.Create(nil);
  Parametros.Tabelas.Create(TTabelaPessoa.Create(Pessoa));

  ParamSQL := TParametroSQL.Create('CLIENTES C');
  ParamSQL.AdicionarCamposPk('ID', 'C.CODIGO');
  ParamSQL.ListaCampos.AddCampo('NOME', 'C.NOME');
  ParamSQL.ListaCampos.AddCampo('CPF', 'C.CPF', nil, '', '', [TFuncoes.SoNumeros]);

  Parametros.AddParametro(ParamSQL);

  // Registrar callback
  Parametros.ListaCallbacks.Add(ValidarCPF);

  // Executar conversão...
end;
```

### Como os callbacks são executados (fluxo)

Durante a execução de `TConversao` (resumo):

1. Antes de processar o bloco SQL: callbacks `PreProc`
2. Para cada registro:
   - `PreExec` (antes de mapear/gravar)
   - aplicação de funções / lookups / mapeamentos
   - gravação no destino (INSERT/UPDATE)
   - `PosExec` (após operar no registro)
3. Ao terminar o bloco: `PosProc`
4. Callbacks `Validacao` podem ser disparados durante PreExec ou em um momento específico de verificação, de acordo com implementação

Os callbacks recebem contexto (geralmente instância de `TConversao`, `TParametrosConversao` ou outro objeto) para acessar `cdsOrigem`, `cdsDestino`, parâmetros e utilitários.

---

## Exemplo completo de callback na conversão (validação + ajuste)

Objetivo: validar CPF e marcar erro em um campo virtual sem interromper toda a conversão (apenas registrar o erro no log).

```delphi
function ValidarCPF_e_MarcarErro: TCallbackConversao;
begin
  Result := TCallbackConversao.Create(
    procedure(Conversao: TConversao)
    begin
      var CPF := Conversao.Parametros.cdsOrigem.FieldByName('CPF').AsString;
      try
        if not Geral.ValidarCPF(CPF) then
        begin
          // Registrar erro no log central do processo
          Conversao.Util.LogAdd('CPF inválido: ' + CPF);

          // Marcar campo virtual no cdsDestino (se existir)
          if Conversao.Parametros.cdsDestino.FindField('ERRO_VALIDACAO') <> nil then
            Conversao.Parametros.cdsDestino.FieldByName('ERRO_VALIDACAO').AsString := 'CPF inválido';
        end;
      except
        on E: Exception do
          Conversao.Util.LogAdd('Erro no callback ValidarCPF: ' + E.Message);
      end;
    end,
    TMomentoConversao.PosExec
  );
end;
```

Notas:
- Esse callback não levanta exceção, apenas registra — comportamento útil quando se quer continuar com a conversão e revisar erros depois.
- Evite executar operações longas dentro do callback (ex.: chamadas HTTP síncronas) — isso prejudica performance.

---

## Uso do LocalizarBinário em callbacks (buscas em TClientDataSet)

Ao procurar registros em `TClientDataSet` dentro de um callback, use `LocalizarBinário` e garanta que o dataset esteja ordenado pelo campo utilizado:

```delphi
procedure AjustarReferencias(Conversao: TConversao);
begin
  var cdsAux := Conversao.Parametros.cdsAuxiliar; // exemplo
  cdsAux.IndexFieldNames := 'CODIGO';
  if LocalizarBinario(cdsAux, 'CODIGO', Conversao.Parametros.cdsOrigem.FieldByName('COD_REF').AsString) then
  begin
    // encontrou, ajustar mapeamento
  end;
end;
```

---

## Callbacks no Framework de Integrações

No Framework/Integracoes, callbacks são usados para ações como logs, auditoria, notificação de status e resumo de integração. Geralmente são métodos da interface `IIntegracaoPlataforma` ou funções registradas no fluxo.

### Exemplos práticos

1. Registrar log sempre que um status é alterado:

```delphi
procedure NotificarStatus(Integracao: IIntegracaoPlataforma; Status: TStatusIntegracao);
begin
  Integracao.Handshake(Status, 'Empresa X', 'ProcessoY', 'Resumo...');
  Integracao.LogAdd(Format('Status alterado: %d', [Ord(Status)]));
end;
```

2. Incrementar resumo de integração (TResumoIntegracao) via callback:

```delphi
procedure AtualizarResumoRecebidos(Resumo: TResumoIntegracao; Recebidos: Integer);
begin
  Resumo.IncRecebidos(Recebidos);
end;

// Uso dentro do processamento:
var Resumo: TResumoIntegracao;
begin
  Resumo := TResumoIntegracao.Create;
  try
    // para cada item recebido:
    AtualizarResumoRecebidos(Resumo, 1);
  finally
    Integracao.LogAdd(Resumo.ToString(True));
    Resumo.Free;
  end;
end;
```

3. Auditar operações críticas:

```delphi
Integracao.GravarAuditoria('PedidoRecebido', JsonDoPedido, Now);
```

---

## Boas práticas e recomendações

- Nomeie callbacks descritivamente: ValidarCPF_PreExec, AjustarPreco_PosExec, Auditoria_PedidoPosExec.
- Documente o momento (TMomentoConversao) e efeitos colaterais do callback.
- Mantenha callbacks curtos e com responsabilidade única (princípio SRP).
- Evite operações longas (I/O, HTTP) dentro de callbacks; se necessário, execute assíncrono ou delegue a um worker.
- Evite commits/rollbacks dentro de callbacks quando o fluxo externo controla transações — prefira sinalizar problemas e deixar a camada de orquestração decidir.
- Se precisar acessar datasets, use métodos seguros:
  - Use `LocalizarBinário` com `IndexFieldNames` configurado.
  - Verifique se o dataset não é nil e se os campos existem antes de acessar.
- Seja cuidadoso com concorrência (threads): callbacks geralmente rodam no mesmo thread do processamento; se usar multithreading, proteja recursos compartilhados.

---

## Tratamento de erros em callbacks

- Estratégia 1 — Falha crítica: levante uma exceção específica (EFalhaValidacao / EExcecaoUsuario). O orquestrador pode abortar o processamento e registrar o erro.
- Estratégia 2 — Falha não crítica: capture exceção no callback, registre no log e continue (útil para validações que não inválidam o registro).
- Exemplo de captura:

```delphi
procedure CallbackSeguro(Conversao: TConversao);
begin
  try
    // lógica do callback
  except
    on E: Exception do
    begin
      Conversao.Util.LogAdd('Erro no callback: ' + E.Message);
      // opcional: Conversao.Util.AdicionarErro(E.Message);
    end;
  end;
end;
```

- Evite deixar exceções não tratadas em callbacks se o objetivo for continuar a operação.

---

## Transações e callbacks

- O TConversao controla transações (IniciarTransacao / Commit / Rollback).
- Não dê commit/rollback manualmente dentro de callbacks, a menos que tenha certeza do impacto (por exemplo, sub-transações suportadas).
- Se um callback precisa persistir informações auxiliares, prefira usar um mecanismo de log/auditoria que seja compatível com a transação principal.

---

## Testes e debugging

- Unit tests:
  - Crie stubs/mocks para `TConversao`, `TParametrosConversao` e `IIntegracaoPlataforma`.
  - Teste callbacks isoladamente: chame o procedimento com um `TConversao` fake que expõe `cdsOrigem`/`métodos de log`.
- Testes de integração:
  - Configure um ambiente de teste com DB de sandbox e rode conversões completas.
  - Ative logs e verifique que callbacks executaram nas etapas corretas.
- Debugging:
  - Coloque logs no início/fim do callback: `LogAdd('Entrou em ValidarCPF')`
  - Use pontos de interrupção (breakpoints) em callbacks anônimos e métodos registrados.
  - Verifique `mmoErros` e arquivos de log gerados automaticamente pelo sistema.
- Exemplo de teste simples (pseudo-code):

```delphi
procedure TestValidarCPF;
var
  ConversaoFake: TConversaoFake;
  Callback: TCallbackConversao;
begin
  ConversaoFake := TConversaoFake.Create;
  try
    ConversaoFake.SetupCdsOrigemWithField('CPF', '12345678900');
    Callback := ValidarCPF;
    try
      Callback.Exec(ConversaoFake);
      Assert(False, 'Deveria ter falhado para CPF inválido');
    except
      on E: EFailValidacao do
        Assert(True);
    end;
  finally
    ConversaoFake.Free;
  end;
end;
```

---

## Armadilhas comuns

- Acessar campos inexistentes em cdsOrigem/cdsDestino (verifique com FindField antes).
- Modificar coleções (ex.: ListaCallbacks) enquanto está iterando sobre elas — crie uma cópia se necessário.
- Registrar callbacks com efeitos colaterais que dependem da ordem. Quando a ordem importa, documente e garanta a inserção na ordem correta.
- Executar chamadas externas síncronas (HTTP) em cada registro sem batching — matou performance.

---

## Checklist rápido para criar um callback no Sol.NET

1. Defina claramente o objetivo e momento de execução.
2. Escolha o tipo: anônimo, função que retorna TCallbackConversao ou método encapsulado.
3. Acesse apenas o contexto necessário (cdsOrigem/cdsDestino/utilitários).
4. Trate exceções internamente ou levante exceção específica (decida política).
5. Registre no Parametros.ListaCallbacks no lugar correto.
6. Teste isoladamente (unit) e com um bloco de conversão (integração).
7. Documente nome, momento e possíveis efeitos colaterais.

---

## Referências rápidas (API / nomes usados no projeto)

- Classe: TCallbackConversao
- Enum momentos: TMomentoConversao (PreProc, PreExec, Validacao, PosExec, PosProc)
- Utilitários: Conversao.Util.LogAdd, Conversao.Util.AdicionarErro
- Integrações: IIntegracaoPlataforma.Handshake, LogAdd, GravarAuditoria
- Resumo: TResumoIntegracao.IncRecebidos/IncEnviados/ToString

---

Se quiser, eu gero:
- Trechos prontos para colar na sua aplicação (funções ValidarCPF/MarcarErro/AjustarPreco) já adaptados com o padrão do seu projeto (variáveis inline, LocalizarBinário, uso de TFuncoes).
- Um exemplo de teste unitário completo em Delphi para um callback.
- Um template de README para guiar outros desenvolvedores a registrar callbacks em novas conversões ou integrações.

Deseja que eu gere algum desses exemplos prontos?
