# 📚 50 Exercícios Práticos de Delphi - Sol.NET

## 🎯 Nível Básico (Exercícios 1–15)

### 1. Crie um formulário com dois `TEdit` e um `TButton`. Ao clicar no botão, concatene os valores dos dois `TEdit` e exiba o resultado em um `ShowMessage`.
```pascal
unit uFrmPraticaSolnet;

interface

uses
  Windows, Messages, SysUtils, System.Math, Variants, Classes, Graphics, DateUtils,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, UnitExecutaBotao, uFuncoes;

type
  TForm1 = class(TForm)
    edt1: TEdit;
    edt2: TEdit;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Ex1ConcatenarTextos;

  public
    { Public declarations }
  end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Ex1ConcatenarTextos;
var
    TextoFinal: string;

begin
  TextoFinal := edt1.text + edt2.text;
  ShowMessage('Resultado: ' + TextoFinal);

end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex1ConcatenarTextos);
end;

end.
```
### 2. Desenvolva uma calculadora simples com quatro operações básicas (`+`, `-`, `*`, `/`) utilizando `TEdit` para entrada de valores e `TButton` para cada operação.
```pascal
procedure TForm1.Ex2CalculadoraBasica;
var
    num1, num2: Double;
  resultado: Double;
begin

  num1 := StrToFloatDef(edt1.text, 0);
  num2 := StrToFloatDef(edt2.text, 0);

  memo1.clear;

  resultado := num1 + num2;
  memo1.Lines.Add('Soma: ' + FloatToStr(resultado));

  resultado := num1 - num2;
  memo1.Lines.Add('Subtração: ' + FloatToStr(resultado));

  resultado := num1 * num2;
  memo1.Lines.Add('Multiplicação: ' + FloatToStr(resultado));

  if num2 <> 0 then
  begin
    resultado := num1 / num2;
    memo1.Lines.Add('Divisão: ' + FloatToStr(resultado));
  end
  else
    memo1.Lines.Add('Divisão: Erro - divisão por zero');
end;
```
  
### 3. Crie um conversor de temperatura que transforme Celsius em Fahrenheit e Kelvin, exibindo os três valores simultaneamente em `TLabel`.
  ```pascal
procedure TForm1.Ex3ConversorTemp;
const
    TEMPERATURA_INFORMADA = 25;
var
    celsius, fahrenheit, kelvin: Double;
begin
  celsius := TEMPERATURA_INFORMADA;
  fahrenheit := celsius * 9 / 5 + 32;
  kelvin := celsius + 273.15;

    lbl1.Caption := Format('Celsius: %.2f °C' + #13#10 + 'Fahrenheit: %.2f °F ' +#13#10+ 'Kelvin: %.2f K',
    [celsius, fahrenheit, kelvin]);

end;
```
   
## 4. Implemente um validador de CPF que receba o CPF em um `TEdit`, remova caracteres especiais usando `TFuncoes.SoNumeros` e valide o dígito verificador.
```pascal
procedure TForm1.Ex4ValidadorCpf;
var
    cpf: string;
  i, soma, resto, digito1, digito2: Integer;
  valido: Boolean;
begin

  cpf := TFuncoes.SoNumeros(edt1.text);

  if Length(cpf) <> 11 then
  begin
    ShowMessage('CPF inválido: deve conter 11 dígitos.');
    Exit;
  end;

  if cpf = StringOfChar(cpf[1], 11) then
  begin
    ShowMessage('CPF inválido: todos os dígitos iguais.');
    Exit;
  end;

  soma := 0;
  for i := 1 to 9 do
    soma := soma + StrToInt(cpf[i]) * (11 - i);

  resto := soma mod 11;
  if resto < 2 then
    digito1 := 0
  else
    digito1 := 11 - resto;

  soma := 0;
  for i := 1 to 10 do
    soma := soma + StrToInt(cpf[i]) * (12 - i);

  resto := soma mod 11;
  if resto < 2 then
    digito2 := 0
  else
    digito2 := 11 - resto;

  valido := (digito1 = StrToInt(cpf[10])) and (digito2 = StrToInt(cpf[11]));

  if valido then
    ShowMessage('CPF válido!')
  else
    ShowMessage('CPF inválido!');
end;
```
## 5. Desenvolva um formulário que receba uma data em `TEdit` e calcule quantos dias faltam para o próximo aniversário da pessoa.
```pascal
procedure TForm1.Ex5ProximoAniversario;
var
    DataNasc, Hoje, ProxAniv: TdateTime;
  IdadeAnos, DiasVida, DiasParaAniv, MesesParaAniv: Integer;
  DiaSemanaNasc, DiaSemanaAniv: string;
begin
  memo1.clear;

  if not TryStrToDate(edt1.text, DataNasc) then
  begin
    memo1.Lines.Add('Data de nascimento inválida!');
    Exit;
  end;

  Hoje := Date;

  IdadeAnos := YearsBetween(Hoje, DataNasc);

  ProxAniv := RecodeYear(DataNasc, YearOf(Hoje));
  if ProxAniv < Hoje then
    ProxAniv := RecodeYear(DataNasc, YearOf(Hoje) + 1);

  DiasParaAniv := DaysBetween(Hoje, ProxAniv);
  MesesParaAniv := MonthsBetween(ProxAniv, Hoje) * -1;

  DiaSemanaNasc := FormatDateTime('dddd', DataNasc);

  DiaSemanaAniv := FormatDateTime('dddd', ProxAniv);

  DiasVida := DaysBetween(Hoje, DataNasc);

  memo1.Lines.Add('Data de nascimento: ' + FormatDateTime('dd/mm/yyyy', DataNasc));
  memo1.Lines.Add('Idade: ' + IntToStr(IdadeAnos) + ' anos');
  memo1.Lines.Add('Próximo aniversário: ' + FormatDateTime('dd/mm/yyyy', ProxAniv) +
    ' (' + DiaSemanaAniv + ')');
  memo1.Lines.Add('Faltam ' + IntToStr(DiasParaAniv) + ' dias para seu aniversário');
  memo1.Lines.Add('Faltam ' + IntToStr(MesesParaAniv) + ' meses para seu aniversário');
  memo1.Lines.Add('Você nasceu em uma ' + DiaSemanaNasc);
  memo1.Lines.Add('Você já viveu ' + IntToStr(DiasVida) + ' dias!');
end;
```

6. Crie uma aplicação que converta um valor monetário digitado em extenso (ex: `1234.56 → "Um mil duzentos e trinta e quatro reais e cinquenta e seis centavos"`).
```pascal
function TForm1.NumeroParaExtenso(Numero: Integer): string;
const
  Unidades: array[0..9] of string = ('', 'um', 'dois', 'três', 'quatro', 
    'cinco', 'seis', 'sete', 'oito', 'nove');
  Dezenas: array[0..9] of string = ('', 'dez', 'vinte', 'trinta', 'quarenta',
    'cinquenta', 'sessenta', 'setenta', 'oitenta', 'noventa');
  Especiais: array[10..19] of string = ('dez', 'onze', 'doze', 'treze', 
    'quatorze', 'quinze', 'dezesseis', 'dezessete', 'dezoito', 'dezenove');
  Centenas: array[0..9] of string = ('', 'cento', 'duzentos', 'trezentos',
    'quatrocentos', 'quinhentos', 'seiscentos', 'setecentos', 'oitocentos', 'novecentos');
var
  milhar, centena, dezena, unidade: Integer;
  resultado: string;
begin
  if Numero = 0 then
  begin
    Result := 'zero';
    Exit;
  end;
  
  if Numero = 100 then
  begin
    Result := 'cem';
    Exit;
  end;
  
  resultado := '';
  
  milhar := Numero div 1000;
  centena := (Numero mod 1000) div 100;
  dezena := (Numero mod 100) div 10;
  unidade := Numero mod 10;
  
  if milhar > 0 then
  begin
    if milhar = 1 then
      resultado := 'mil'
    else
      resultado := NumeroParaExtenso(milhar) + ' mil';
  end;
  
  if centena > 0 then
  begin
    if resultado <> '' then
      resultado := resultado + ' ';
    resultado := resultado + Centenas[centena];
  end;
  
  if (dezena = 1) and (unidade > 0) then
  begin
    if resultado <> '' then
      resultado := resultado + ' e ';
    resultado := resultado + Especiais[10 + unidade];
  end
  else
  begin
    if dezena > 0 then
    begin
      if resultado <> '' then
        resultado := resultado + ' e ';
      resultado := resultado + Dezenas[dezena];
    end;
    
    if unidade > 0 then
    begin
      if resultado <> '' then
        resultado := resultado + ' e ';
      resultado := resultado + Unidades[unidade];
    end;
  end;
  
  Result := resultado;
end;

procedure TForm1.Ex6ConversorNumeroParaString;
var
  valor: Double;
  reais, centavos: Integer;
  extenso: string;
begin
  if not TryStrToFloat(edt1.Text, valor) then
  begin
    ShowMessage('Digite um valor válido!');
    Exit;
  end;
  
  if valor < 0 then
  begin
    ShowMessage('Valor não pode ser negativo!');
    Exit;
  end;
  
  reais := Trunc(valor);
  centavos := Round((valor - reais) * 100);
  
  extenso := '';
  
  if reais > 0 then
  begin
    extenso := NumeroParaExtenso(reais);
    if reais = 1 then
      extenso := extenso + ' real'
    else
      extenso := extenso + ' reais';
  end
  else
    extenso := 'zero reais';
  
  if centavos > 0 then
  begin
    extenso := extenso + ' e ' + NumeroParaExtenso(centavos);
    if centavos = 1 then
      extenso := extenso + ' centavo'
    else
      extenso := extenso + ' centavos';
  end;
  
  ShowMessage(extenso);
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex6ConversorNumeroParaString);
end;
```  
7. Implemente um gerador de senhas aleatórias com opções de tamanho (6–20 caracteres) e inclusão de números, letras maiúsculas, minúsculas e caracteres especiais.
```pascal
function TForm1.GeradorSenhaForte(Senha: string): Boolean;
var
    temNumero, temMaiuscula, temMinuscula: Boolean;
  i: Integer;
begin
  temNumero := False;
  temMaiuscula := False;
  temMinuscula := False;

  for i := 1 to Length(Senha) do
  begin
    if Senha[i] in ['0' .. '9'] then
      temNumero := True
    else if Senha[i] in ['A' .. 'Z'] then
      temMaiuscula := True
    else if Senha[i] in ['a' .. 'z'] then
      temMinuscula := True;
  end;

  Result := (Length(Senha) >= 6) and (Length(Senha) <= 20) and temNumero and temMaiuscula and temMinuscula;
end;

procedure TForm1.Ex7GeradorSenha;
var
    Senha: string;
begin
  memo1.clear;
  Senha := edt1.text;

  if GeradorSenhaForte(Senha) then
    memo1.Lines.Add('✅ Senha forte!')
  else
  begin
    memo1.Lines.Add('❌ Senha fraca. Faltam os seguintes critérios:');
    if Length(Senha) < 6 then
      memo1.Lines.Add(' - MINIMO DE 6 CARACTERES');
    if Length(Senha) > 20 then
      memo1.Lines.Add(' - MAXIMO DE 20 CARACTERES');

    if not(Senha.Contains('0') or Senha.Contains('1') or Senha.Contains('2') or
      Senha.Contains('3') or Senha.Contains('4') or Senha.Contains('5') or
      Senha.Contains('6') or Senha.Contains('7') or Senha.Contains('8') or
      Senha.Contains('9')) then
      memo1.Lines.Add('- Pelo menos 1 número');

    if Senha = LowerCase(Senha) then
      memo1.Lines.Add('- Pelo menos 1 letra maiúscula');

    if Senha = UpperCase(Senha) then
      memo1.Lines.Add('- Pelo menos 1 letra minúscula');

  end;

end;

```

8. Desenvolva um contador regressivo visual usando `TTimer` que inicie em um valor definido pelo usuário e exiba o tempo restante em um `TLabel`.
9. Crie um formulário que receba um texto em `TMemo` e conte: total de caracteres, palavras, linhas e vogais.
10. Implemente uma calculadora de IMC (Índice de Massa Corporal) que classifique o resultado em: abaixo do peso, peso normal, sobrepeso ou obesidade.
11. Desenvolva um sistema que receba uma lista de 10 números em um `TMemo` (um por linha) e exiba o maior, menor e a média em `TLabel` separados.
12. Crie um validador de email que verifique se o formato está correto (presença de `@`, domínio válido, etc.).
13. Implemente um conversor de bases numéricas (decimal, binário, octal, hexadecimal) com validação de entrada.
14. Desenvolva um formulário que calcule o valor de parcelas de um financiamento com base no valor total, taxa de juros e número de parcelas.
15. Crie um sistema que receba duas datas e calcule: diferença em dias, meses, anos e dia da semana de cada data.

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
