# 📚 50 Exercícios Práticos de Delphi - Sol.NET

## 🎯 Nível Básico (Exercícios 1–15)

### 1. Crie um formulário com dois `TEdit` e um `TButton`. Ao clicar no botão, concatene os valores dos dois `TEdit` e exiba o resultado em um `ShowMessage`.
```pascal
unit Unit1;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, System.Classes;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure ConcatenarTextos;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ConcatenarTextos;
end;

procedure TForm1.ConcatenarTextos;
var
  TextoFinal: string;
begin
  TextoFinal := Edit1.Text + Edit2.Text;
  ShowMessage('Resultado: ' + TextoFinal);
end;

end.
```
### 2. Desenvolva uma calculadora simples com quatro operações básicas (`+`, `-`, `*`, `/`) utilizando `TEdit` para entrada de valores e `TButton` para cada operação.
```pascal
unit uFrmPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, UnitExecutaBotao;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    edt1: TEdit;
    edt2: TEdit;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
  private
    procedure Soma;
    procedure Subtracao;
    procedure Multiplicacao;
    procedure Divisao;
    function ObterValores(out A, B: Double): Boolean;
  public
  end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.ObterValores(out A, B: Double): Boolean;
begin
  Result := TryStrToFloat(edt1.Text, A) and TryStrToFloat(edt2.Text, B);
  if not Result then
    ShowMessage('Digite dois números válidos.');
end;

procedure TForm1.Soma;
var
    A, B: Double;
begin
  if ObterValores(A, B) then
    ShowMessage('Resultado: ' + FloatToStr(A + B));
end;

procedure TForm1.Subtracao;
var
    A, B: Double;
begin
  if ObterValores(A, B) then
    ShowMessage('Resultado: ' + FloatToStr(A - B));
end;

procedure TForm1.Multiplicacao;
var
    A, B: Double;
begin
  if ObterValores(A, B) then
    ShowMessage('Resultado: ' + FloatToStr(A * B));
end;

procedure TForm1.Divisao;
var
    A, B: Double;
begin
  if ObterValores(A, B) then
  begin
    if B = 0 then
      ShowMessage('Erro: divisão por zero.')
    else
      ShowMessage('Resultado: ' + FloatToStr(A / B));
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Soma);
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  ExecutarBotao(Subtracao);
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  ExecutarBotao(Multiplicacao);
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  ExecutarBotao(Divisao);
end;

end.
```
  
### 3. Crie um conversor de temperatura que transforme Celsius em Fahrenheit e Kelvin, exibindo os três valores simultaneamente em `TLabel`.
  ```pascal
unit uFrmPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, UnitExecutaBotao;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);

  private
    { Private }
    procedure Conversor;

  public
    { Public }
  end;

var
    Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Conversor;
const TEMPERATURA_INFORMADA = 25;
var
    celsius, fahrenheit, kelvin: Double;
begin
  celsius := TEMPERATURA_INFORMADA;
  fahrenheit := celsius * 9 / 5 + 32;
  kelvin := celsius + 273.15;

  lbl1.Caption := Format('Celsius: %.2f °C | Fahrenheit: %.2f °F | Kelvin: %.2f K',
    [celsius, fahrenheit, kelvin]);

end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Conversor);
end;

end.
```
   
## 4. Implemente um validador de CPF que receba o CPF em um `TEdit`, remova caracteres especiais usando `TFuncoes.SoNumeros` e valide o dígito verificador.
```pascal
unit uFrmPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, UnitExecutaBotao, uFuncoes;

type
  TForm1 = class(TForm)
    edt1: TEdit;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    procedure ValidadorCpf;
  public
  end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ValidadorCpf;
var
    cpf: string;
  i, soma, resto, digito1, digito2: Integer;
  valido: Boolean;
begin
  // Remove caracteres especiais
  cpf := TFuncoes.SoNumeros(edt1.Text);

  // Verifica se tem 11 dígitos
  if Length(cpf) <> 11 then
  begin
    ShowMessage('CPF inválido: deve conter 11 dígitos.');
    Exit;
  end;

  // Verifica se todos os dígitos são iguais
  if cpf = StringOfChar(cpf[1], 11) then
  begin
    ShowMessage('CPF inválido: todos os dígitos iguais.');
    Exit;
  end;

  // Cálculo do primeiro dígito verificador
  soma := 0;
  for i := 1 to 9 do
    soma := soma + StrToInt(cpf[i]) * (11 - i);

  resto := soma mod 11;
  if resto < 2 then
    digito1 := 0
  else
    digito1 := 11 - resto;

  // Cálculo do segundo dígito verificador
  soma := 0;
  for i := 1 to 10 do
    soma := soma + StrToInt(cpf[i]) * (12 - i);

  resto := soma mod 11;
  if resto < 2 then
    digito2 := 0
  else
    digito2 := 11 - resto;

  // Verifica se os dígitos calculados coincidem com os informados
  valido := (digito1 = StrToInt(cpf[10])) and (digito2 = StrToInt(cpf[11]));

  if valido then
    ShowMessage('CPF válido!')
  else
    ShowMessage('CPF inválido!');
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(ValidadorCpf);
end;

end.
```
## 5. Desenvolva um formulário que receba uma data em `TEdit` e calcule quantos dias faltam para o próximo aniversário da pessoa.
```pascal
unit uFrmPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, DateUtils,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, UnitExecutaBotao, uFuncoes;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    edt1: TEdit;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    procedure data;
  public
    { Public declarations }
  end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.data;
var
    DataAtual, DataNascimento, ProximoAniversario: TDate;
  DiasRestantes: Integer;
begin
  try
    DataNascimento := StrToDate(edt1.Text);
    DataAtual := Date;

    ProximoAniversario := EncodeDate(YearOf(DataAtual), 
      MonthOf(DataNascimento), 
      DayOf(DataNascimento));

    if ProximoAniversario < DataAtual then
      ProximoAniversario := IncYear(ProximoAniversario, 1);

    DiasRestantes := DaysBetween(DataAtual, ProximoAniversario);

    ShowMessage('Faltam ' + IntToStr(DiasRestantes) + ' dias para o próximo aniversário!');

  except
    on E: EConvertError do
      ShowMessage('Data inválida! Use o formato dd/mm/yyyy');
    on E: Exception do
      ShowMessage('Erro: ' + E.Message);
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(data);
end;

end.
```

7. Crie uma aplicação que converta um valor monetário digitado em extenso (ex: `1234.56 → "Um mil duzentos e trinta e quatro reais e cinquenta e seis centavos"`).
8. Implemente um gerador de senhas aleatórias com opções de tamanho (6–20 caracteres) e inclusão de números, letras maiúsculas, minúsculas e caracteres especiais.
9. Desenvolva um contador regressivo visual usando `TTimer` que inicie em um valor definido pelo usuário e exiba o tempo restante em um `TLabel`.
10. Crie um formulário que receba um texto em `TMemo` e conte: total de caracteres, palavras, linhas e vogais.
11. Implemente uma calculadora de IMC (Índice de Massa Corporal) que classifique o resultado em: abaixo do peso, peso normal, sobrepeso ou obesidade.
12. Desenvolva um sistema que receba uma lista de 10 números em um `TMemo` (um por linha) e exiba o maior, menor e a média em `TLabel` separados.
13. Crie um validador de email que verifique se o formato está correto (presença de `@`, domínio válido, etc.).
14. Implemente um conversor de bases numéricas (decimal, binário, octal, hexadecimal) com validação de entrada.
15. Desenvolva um formulário que calcule o valor de parcelas de um financiamento com base no valor total, taxa de juros e número de parcelas.
16. Crie um sistema que receba duas datas e calcule: diferença em dias, meses, anos e dia da semana de cada data.

---

## 🔧 Nível Intermediário (Exercícios 16–35)

16. Desenvolva um CRUD completo (Create, Read, Update, Delete) para uma tabela de Clientes usando `TFDQuery` e `TFDConnection` com Firebird.
17. Implemente um sistema de login que valide usuário e senha contra uma tabela no banco de dados, criptografando a senha com MD5.
18. Crie um formulário de cadastro de produtos com validação de código de barras (EAN-13) incluindo cálculo do dígito verificador.
19. Desenvolva um relatório de vendas usando `TFDQuery` que agrupe por período, categoria de produto e vendedor, exibindo totais em um `TDBGrid`.
20. Implemente um sistema de estoque que controle entradas e saídas de produtos, atualizando o saldo automaticamente e impedindo vendas com estoque zerado.
21. Crie um importador de arquivo CSV que leia dados de clientes e insira automaticamente em uma tabela usando transações.
22. Desenvolva um exportador de dados que gere arquivo XML com estrutura hierárquica a partir de uma consulta SQL (Pedidos → Itens).
23. Implemente um sistema de backup automático do banco de dados Firebird que execute em horários agendados usando `TTimer`.
24. Crie um formulário de pesquisa avançada de produtos com filtros dinâmicos (nome, categoria, faixa de preço) construindo SQL dinamicamente.
25. Desenvolva um sistema de auditoria que registre todas as alterações (insert, update, delete) em uma tabela de log com data, hora, usuário e valores anteriores/novos.
26. Implemente um gerador de código de barras que converta um código numérico em imagem Code128 ou EAN-13 e exiba em um `TImage`.
27. Crie um sistema de contas a pagar/receber com controle de vencimento, juros, multa e baixa de títulos.
28. Desenvolva um módulo de integração REST que consuma uma API externa (ex: ViaCEP) e preencha automaticamente campos de endereço ao digitar o CEP.
29. Implemente um sistema de impressão de etiquetas personalizadas com preview, usando componentes de relatório (ex: QuickReport ou FastReport).
30. Crie um dashboard com gráficos (`TChart`) exibindo: vendas por mês, produtos mais vendidos e ticket médio.
31. Desenvolva um sistema de permissões de usuário que controle acesso a formulários e funcionalidades específicas baseado em perfis (Admin, Vendedor, Caixa).
32. Implemente um sincronizador de dados que compare duas tabelas (local e servidor) e aplique apenas as diferenças (delta sync).
33. Crie um módulo de envio de emails automáticos usando Indy (`IdSMTP`) com anexos e templates HTML.
34. Desenvolva um sistema de controle de versão de banco de dados que execute scripts SQL sequenciais para atualização de estrutura.
35. Implemente um gerenciador de filas de processamento assíncrono que execute tarefas em background sem travar a interface.

---

## 🚀 Nível Avançado (Exercícios 36–50)

36. Desenvolva um framework de conversão de dados genérico similar ao `ConversaoBuilder` do Sol.NET, com suporte a mapeamento fluente de campos e callbacks.
37. Implemente um sistema de integração fiscal completo que envie NFC-e para SEFAZ, valide retorno e armazene XML assinado.
38. Crie um módulo de sincronização PDV-Servidor que implemente controle de conflitos, versionamento e retry automático com backoff exponencial.
39. Desenvolva um motor de regras de negócio configurável via banco de dados que execute validações dinâmicas sem recompilar a aplicação.
40. Implemente um sistema de multi-tenancy que isole dados por empresa usando schemas ou filtros automáticos em todas as queries.
41. Crie um framework de integrações seguindo o padrão Strategy do Framework de Integrações do Sol.NET, com suporte a múltiplos provedores.
42. Desenvolva um sistema de cache distribuído usando Redis ou Memcached para otimizar consultas frequentes.
43. Implemente um módulo de Business Intelligence que processe OLAP Cubes e permita análise dimensional de vendas.
44. Crie um sistema de monitoramento em tempo real que exiba status de integrações, filas de processamento e alertas críticos.
45. Desenvolva um gerador de relatórios dinâmicos que permita ao usuário criar consultas SQL via interface visual drag-and-drop.
46. Implemente um sistema de replicação de dados entre Firebird e SQL Server mantendo sincronização bidirecional.
47. Crie um módulo de machine learning básico que classifique produtos automaticamente com base em descrição usando TensorFlow ou similar.
48. Desenvolva um sistema de workflow configurável que permita criar fluxos de aprovação personalizados para diferentes processos.
49. Implemente um gateway de pagamentos que integre múltiplos adquirentes (Stone, Cielo, Rede) usando padrão Factory e Strategy.
50. Crie uma aplicação completa de PDV off-line que sincronize vendas, estoque e clientes com servidor central, implementando controle transacional robusto, gestão de exceções customizadas (`EExcecaoUsuario`, `EExcecaoDesenvolvedor`), logging detalhado e interface responsiva seguindo todos os padrões arquiteturais do Sol.NET.

---

## ✨ Observações

- **Exercícios 1–15**: Foco em lógica, componentes básicos e funções nativas  
- **Exercícios 16–35**: Banco de dados, integrações e funcionalidades intermediárias  
- **Exercícios 36–50**: Arquitetura avançada, frameworks e sistemas complexos  

## 🎓 Progressão sugerida

Complete os exercícios sequencialmente para construir conhecimento gradualmente, sempre aplicando os padrões de código do Sol.NET (Clean Code, variáveis inline, nomenclatura PascalCase).
