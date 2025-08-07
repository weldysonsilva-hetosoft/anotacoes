06/08/25


usando Builder
procedure TFrmSDX.BotaoStatusPessoa;
begin
  var ParametroConversao: TParametrosConversao := TConversaoBuilder.Create
    .SetTabelaConversao(TTabelaPessoaStatus.Create, 'SITUACAOEMP')
    .AddPrimaryKey('IDREG')
    .AddCampo('DESCRICAO', 'NOME')
    .Build;

  ConversaoTpStatusPessoas(ParametroConversao);
end;




Botão Status Pessoa (sintaxe tradicional)

procedure TFrmSDX.BotaoStatusPessoa;
begin
  var ParametroConversao: TParametrosConversao := TParametrosConversao.Create(nil);
  ParametroConversao.Tabelas.Create(TTabelaPessoaStatus.Create);

  var ParametroSql: TParametroSQL := TParametroSQL.Create('SITUACAOEMP');
  ParametroSql.AdicionarCamposPk('ID_PESSOA_TP_STATUS', 'IDREG');

  with ParametroSql.ListaCampos do
  begin
    AddCampo('DESCRICAO', 'NOME');
  end;

  ParametroConversao.AddParametro(ParametroSql);

  ConversaoTpStatusPessoas(ParametroConversao);
end;



PASSEI O BOTÃO QUE ESTÁ DANDO ERRO PARA A FORMA BUILDER E O ERRO PERSISTE. 
