# Conversões do 0 — Unit `uFrmABMolas.pas`

## Estrutura da Unit

```pascal
unit uFrmABMolas;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFrmConversao, Data.DB, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons, GeneralEdits, Vcl.Grids, Vcl.DBGrids,
  DBGridPlus, ComboBoxPlus, Vcl.ExtCtrls, RadioGroupPlus, Vcl.Mask;

type
  TFrmABMolas = class(TFrmConversao)
    tabPessoas: TTabSheet;
    tabProdutos: TTabSheet;
    btnPessoas: TBitBtn;
    procedure btnPessoasClick(Sender: TObject);
  private
    { Private declarations }
    // DECLARE OS BOTOES QUE SERÃO UTILIZADOS E CLIQUE EM CIMA DELE APERTANDO CRTL + SHIFT + C ELE IRA AUTOMATICAMENTE DECLARAR OS PROCEDURES NO FINAL DO CODIGO LINHAS 67 A 86
    procedure BotaoClientes;
    procedure BotaoFornecedor;
    procedure BotaoFabricante;
    procedure BotaoFuncionario;
  public
    { Public declarations }
  end;

var
    FrmABMolas: TFrmABMolas;

implementation

uses
  ConversaoBuilder, uConversao.TConversaoGenerica, uConversao.TiposAuxiliares ;

{$R *.dfm}

{ TFrmABMolas }

procedure TFrmABMolas.BotaoClientes;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaPessoa.Create(Cliente),  'tbClientes')
      .AddPrimaryKey('CodigoCliente')
      .AddCampo('NOME', 'RazaoSocial')
      .AddCampo('DESCRICAO', 'NomeFantasia')
      .AddCampo('CPF', 'CnpjCpf')
      .AddCampo('RG', 'IeImRG')
      .AddCampo('VL_LIMITE_CREDITO', 'LimiteCredito')
      .AddCampo('Email', 'Email1')
      .AddCampo('CELULAR', 'Celular1')
      .AddCampo('TELEFONECONTATO', 'CobTelefone2')
      .AddCampo('TELEFONE', 'CobTelefone1')
      .Build;

  var ParametrosEndereco: TParametrosSubConversao := TSubConversaoBuilder.Create
      .SetTabelaConversao(TTabelaEndereco.Create(), 'tbFuncionarios')
      .AddPrimaryKey('CodigoFuncionario')
      .AddCampo('LOGRADOURO', 'Endereco')
      .Build;

  ConversaoPessoas(ParametrosConversao, ParametrosEndereco);
end;

procedure TFrmABMolas.BotaoFabricante;
begin

end;

procedure TFrmABMolas.BotaoFornecedor;
begin

end;

procedure TFrmABMolas.BotaoFuncionario;
begin

end;

procedure TFrmABMolas.btnPessoasClick(Sender: TObject);
begin
  inherited;
    ExecutarBotao(BotaoClientes, Sender);  // COLOCAR ISSO AQUI PARA DA O COMANDO PARA O BOTÃO. AQUI ELE ESTA EXECUTANDO O BOTÃOCLIENTES E SEMPRE COLOCAR O , SENDER.
end;

end.
```

---

## Observações

- **Criação dos Procedures:** Para criar os procedures dos botões, clique sobre o nome deles e pressione `Ctrl + Shift + C` no Delphi. O Delphi irá gerar automaticamente os métodos no final do código.
- **Botão Pessoas:** No `btnPessoasClick`, é executado o método `BotaoClientes` utilizando o comando `ExecutarBotao(BotaoClientes, Sender);`.
- **Conversão de Clientes:** O método `BotaoClientes` monta os parâmetros de conversão de clientes e endereço, e chama `ConversaoPessoas` para realizar a conversão.
- **Procedures de Fabricante, Fornecedor e Funcionário:** Estão declarados mas não implementados. Implemente conforme a necessidade do projeto.

## Dicas

- Sempre que adicionar um novo botão ou procedure, utilize `Ctrl + Shift + C` para gerar o corpo do método.
- As associações de campos entre tabelas são feitas usando os métodos `.AddCampo`.
- Para converter outros tipos (Fabricante, Fornecedor, Funcionário), siga o mesmo modelo utilizado em `BotaoClientes`.

---
