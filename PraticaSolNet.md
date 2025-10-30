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
```pascal

// ESSE PRECISEI DE AJUDA DA IA PARA DESENVOLVER. 
procedure TForm1.Ex8ContadorRegressivo;
var
    minutos: Integer;
begin
  // Obter o valor do usuário (minutos)
  if not TryStrToInt(edt1.text, minutos) or (minutos <= 0) then
  begin
    ShowMessage('Digite um valor válido em minutos (maior que 0)');
    Exit;
  end;

  // Converter minutos para segundos
  tempoRestante := minutos * 60;

  // Configurar e iniciar o timer
  tmr1.Interval := 1000; // 1 segundo
  tmr1.Enabled := True;

  // Atualizar display inicial
  AtualizarDisplayTempo;

  // Feedback para o usuário
  memo1.clear;
  memo1.Lines.Add('Contador regressivo iniciado!');
  memo1.Lines.Add('Tempo total: ' + IntToStr(minutos) + ' minutos');
  memo1.Lines.Add('Pressione o botão novamente para reiniciar');
end;

procedure TForm1.AtualizarDisplayTempo;
var
    minutos, segundos: Integer;
  tempoFormatado: string;
begin
  minutos := tempoRestante div 60;
  segundos := tempoRestante mod 60;

  tempoFormatado := Format('%.2d:%.2d', [minutos, segundos]);

  lbl1.Caption := 'Tempo restante: ' + tempoFormatado;

  // Mudar cor conforme o tempo diminui
  if tempoRestante <= 30 then // últimos 30 segundos
    lbl1.Font.Color := clRed
  else if tempoRestante <= 60 then // último minuto
    lbl1.Font.Color := clMaroon
  else
    lbl1.Font.Color := clBlack;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  // Decrementar o tempo restante
  Dec(tempoRestante);

  // Atualizar o display
  AtualizarDisplayTempo;

  // Verificar se o tempo acabou
  if tempoRestante <= 0 then
  begin
    tmr1.Enabled := False;
    lbl1.Caption := 'TEMPO ESGOTADO!';
    lbl1.Font.Color := clRed;
    ShowMessage('Tempo esgotado!');
    memo1.Lines.Add('--- CONTADOR FINALIZADO ---');
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex8ContadorRegressivo);
end;

end.


(*precisou declarar o  public
    { Public declarations }
  var
    tempoRestante: Integer; No public e mais uma procedure no private
procedure Ex8ContadorRegressivo;
    procedure AtualizarDisplayTempo;*)


```


9. Crie um formulário que receba um texto em `TMemo` e conte: total de caracteres, palavras, linhas e vogais.
```pascal
function TForm1.AnalisarTexto(texto: String; var palavras, vogais, consoantes: Integer): Integer;
var
    i: Integer;
  C: Char;
  emPalavra: Boolean;
begin
  palavras := 0;
  vogais := 0;
  consoantes := 0;
  emPalavra := False;

  for i := 1 to Length(texto) do
  begin
    C := texto[i];


    if C in ['A' .. 'Z', 'a' .. 'z', 'À' .. 'ü'] then
    begin
      if not emPalavra then
      begin
        Inc(palavras);
        emPalavra := True;
      end;
    end
    else
      emPalavra := False;


    if UpCase(C) in ['A', 'E', 'I', 'O', 'U', 'Á', 'É', 'Í', 'Ó', 'Ú'] then
      Inc(vogais)
    else if UpCase(C) in ['A' .. 'Z'] then
      Inc(consoantes);
  end;

  Result := Length(texto);
end;

procedure TForm1.Ex9ContadorCaracter;
var
  texto: String;
  total, palavras, vogais, consoantes, linhas: Integer;
begin

  tmr1.Enabled := False;


  texto := memo1.Text;
  linhas := memo1.Lines.Count;


  total := AnalisarTexto(texto, palavras, vogais, consoantes);


  memo1.Clear;
  memo1.Lines.Add('=== ANÁLISE DO TEXTO ===');
  memo1.Lines.Add('Total de caracteres: ' + IntToStr(total));
  memo1.Lines.Add('Total de palavras: ' + IntToStr(palavras));
  memo1.Lines.Add('Total de linhas: ' + IntToStr(linhas));
  memo1.Lines.Add('Total de vogais: ' + IntToStr(vogais));
  memo1.Lines.Add('Total de consoantes: ' + IntToStr(consoantes));
  memo1.Lines.Add('=======================');
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex9ContadorCaracter);
end;
```

10. Implemente uma calculadora de IMC (Índice de Massa Corporal) que classifique o resultado em: abaixo do peso, peso normal, sobrepeso ou obesidade.
```pascal
procedure TForm1.Ex10CalculadoraIMC;
var
    peso, altura, imc: Double;
  classificacao, corTexto: string;
begin

  if not TryStrToFloat(edt1.text, peso) or (peso <= 0) then
  begin
    ShowMessage('Digite um peso válido em kg (maior que 0)');
    Exit;
  end;

  if not TryStrToFloat(edt2.text, altura) or (altura <= 0) then
  begin
    ShowMessage('Digite uma altura válida em metros (maior que 0)');
    Exit;
  end;

  imc := peso / (altura * altura);

  if imc < 18.5 then
  begin
    classificacao := 'ABAIXO DO PESO';
    corTexto := 'Azul';
    lbl2.Font.Color := clBlue;
  end
  else if imc < 25 then
  begin
    classificacao := 'PESO NORMAL';
    corTexto := 'Verde';
    lbl2.Font.Color := clGreen;
  end
  else if imc < 30 then
  begin
    classificacao := 'SOBREPESO';
    corTexto := 'Laranja';
    lbl2.Font.Color := clWebOrange;
  end
  else if imc < 35 then
  begin
    classificacao := 'OBESIDADE GRAU I';
    corTexto := 'Vermelho';
    lbl2.Font.Color := clRed;
  end
  else if imc < 40 then
  begin
    classificacao := 'OBESIDADE GRAU II';
    corTexto := 'Vermelho Escuro';
    lbl2.Font.Color := clMaroon;
  end
  else
  begin
    classificacao := 'OBESIDADE GRAU III';
    corTexto := 'Vermelho Intenso';
    lbl2.Font.Color := clPurple;
  end;

  lbl1.Caption := 'IMC: ' + FormatFloat('0.00', imc);
  lbl2.Caption := classificacao;

  memo1.clear;
  memo1.Lines.Add('=== CALCULADORA IMC ===');
  memo1.Lines.Add('Peso: ' + FloatToStr(peso) + ' kg');
  memo1.Lines.Add('Altura: ' + FloatToStr(altura) + ' m');
  memo1.Lines.Add('IMC Calculado: ' + FormatFloat('0.00', imc));
  memo1.Lines.Add('Classificação: ' + classificacao);
  memo1.Lines.Add('Cor indicativa: ' + corTexto);
  memo1.Lines.Add('======================');

end;
```

11. Desenvolva um sistema que receba uma lista de 10 números em um `TMemo` (um por linha) e exiba o maior, menor e a média em `TLabel` separados.
```pascal
procedure TForm1.Ex11ListNumber;
var
    i, valor, maior, menor, soma: Integer;
  numeros: array [1 .. 10] of Integer;
begin

  if memo1.Lines.Count <> 10 then
  begin
    ShowMessage('Insira exatamente 10 números, um por linha.');
    Exit;
  end;
  soma := 0;
  maior := StrToIntDef(memo1.Lines[0], 0);
  menor := maior;

  for i := 0 to 9 do
  begin
    valor := StrToIntDef(memo1.Lines[i], 0);
    if valor > maior then
      maior := valor;
    if valor < menor then
      menor := valor;
    soma := soma + valor;
  end;
  lbl1.Caption := 'Maior ' + IntToStr(maior);
  lbl2.Caption := 'Menor ' + IntToStr(menor);
  lbl3.Caption := 'Média ' + FloatToStr(soma / 10);
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex11ListNumber);
end;

```
12. Crie um validador de email que verifique se o formato está correto (presença de `@`, domínio válido, etc.).
```pascal
procedure TForm1.Ex12ValidaEmail;
var
    email: string;
begin
  memo1.clear;
  email := edt1.text;

  if ValidadorEmail(email) then
    memo1.Lines.Add('Emailválido!')
  else
  begin
    memo1.Lines.Add('❌ Email inválido. Faltam os seguintes critérios:');

    if Pos('@', email) = 0 then
      memo1.Lines.Add('- Deve conter "@"');

    if Pos('.', Copy(email, Pos('@', email) + 1, Length(email))) = 0 then
      memo1.Lines.Add('- Domínio deve conter "." após o "@"');

    if Length(email) < 8 then
      memo1.Lines.Add('- Mínimo de 8 caracteres');

    if Length(email) > 25 then
      memo1.Lines.Add('- Máximo de 25 caracteres');

    if not(email.Contains('a') or email.Contains('b') or email.Contains('c') or
      email.Contains('d') or email.Contains('e') or email.Contains('f') or
      email.Contains('g') or email.Contains('h') or email.Contains('i') or
      email.Contains('j') or email.Contains('k') or email.Contains('l') or
      email.Contains('m') or email.Contains('n') or email.Contains('o') or
      email.Contains('p') or email.Contains('q') or email.Contains('r') or
      email.Contains('s') or email.Contains('t') or email.Contains('u') or
      email.Contains('v') or email.Contains('w') or email.Contains('x') or
      email.Contains('y') or email.Contains('z')) then
      memo1.Lines.Add('- Deve conter pelo menos uma letra');
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex12ValidaEmail);
end;

```

13. Implemente um conversor de bases numéricas (decimal, binário, octal, hexadecimal) com validação de entrada.
```pascal
function IntToBin(Value: Integer; Digits: Integer): string;
begin
  Result := '';
  while Value > 0 do
  begin
    Result := Chr(Ord('0') + (Value mod 2)) + Result;
    Value := Value div 2;
  end;
  while Length(Result) < Digits do
    Result := '0' + Result;
end;

function IntToOct(Value: Integer): string;
begin
  Result := '';
  while Value > 0 do
  begin
    Result := Chr(Ord('0') + (Value mod 8)) + Result;
    Value := Value div 8;
  end;
  if Result = '' then
    Result := '0';
end;

procedure TForm1.Ex13ConversorBaseNumericas;
var
    decimal: Integer;
  entrada: string;
begin
  entrada := edt1.text;

  if not TryStrToInt(entrada, decimal) then
  begin
    ShowMessage('Valor inválido para conversão. Digite um número inteiro.');
    Exit;
  end;

  memo1.clear;
  memo1.Lines.Add('Decimal: ' + IntToStr(decimal));
  memo1.Lines.Add('Binário: ' + IntToBin(decimal, 8));
  memo1.Lines.Add('Octal: ' + IntToOct(decimal));
  memo1.Lines.Add('Hexadecimal: ' + IntToHex(decimal, 8));
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex13ConversorBaseNumericas);
end;
```
14. Desenvolva um formulário que calcule o valor de parcelas de um financiamento com base no valor total, taxa de juros e número de parcelas.
```pascal
procedure TForm1.Ex14ParcelasFinanciamento;

var
    valorTotal, taxaJuros, parcelaMensal: Double;
  numParcelas: Integer;
  entradaValor, entradaJuros, entradaParcelas: string;
begin

  entradaValor := edt1.text;
  entradaJuros := edt2.text;
  entradaParcelas := edt3.text;

  if not TryStrToFloat(entradaValor, valorTotal) then
  begin
    ShowMessage('Valor total inválido.');
    Exit;
  end;

  if not TryStrToFloat(entradaJuros, taxaJuros) then
  begin
    ShowMessage('Taxa de juros inválida.');
    Exit;
  end;

  if not TryStrToInt(entradaParcelas, numParcelas) then
  begin
    ShowMessage('Número de parcelas inválido.');
    Exit;
  end;

  taxaJuros := taxaJuros / 100;

  if taxaJuros = 0 then
    parcelaMensal := valorTotal / numParcelas
  else
    parcelaMensal := (valorTotal * taxaJuros) / (1 - Power(1 + taxaJuros, -numParcelas));

  memo1.clear;
  memo1.Lines.Add('Valor total: R$ ' + FormatFloat('0.00', valorTotal));
  memo1.Lines.Add('Taxa de juros: ' + FormatFloat('0.00%', taxaJuros * 100));
  memo1.Lines.Add('Número de parcelas: ' + IntToStr(numParcelas));
  memo1.Lines.Add('Valor da parcela: R$ ' + FormatFloat('0.00', parcelaMensal));
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex14ParcelasFinanciamento);
end;

```

15. Crie um sistema que receba duas datas e calcule: diferença em dias, meses, anos e dia da semana de cada data.
```pascal
procedure TForm1.Ex15SistemasData;
var
    data1, data2: TdateTime;
  Dias, Semanas, Meses, Anos: Integer;
begin
  memo1.clear;

  if not TryStrToDate(edt1.text, data1) then
    ShowMessage('Data 1 inválida! Forneca uma data no formato DD/MM/AAAA');

  if not TryStrToDate(edt2.text, data2) then
    ShowMessage('Data 2 inválida! Forneca uma data no formato DD/MM/AAAA');

  Dias := DaysBetween(data1, data2);
  Semanas := Round(Dias / 7);
  Meses := MonthsBetween(data1, data2);
  Anos := YearsBetween(data1, data2);

  memo1.Lines.Add('Diferença em dias: ' + IntToStr(Dias));
  memo1.Lines.Add('Diferença em semanas (arredondado): ' + IntToStr(Semanas));
  memo1.Lines.Add('Diferença em meses (aproximado): ' + IntToStr(Meses));
  memo1.Lines.Add('Diferença em anos (aproximado): ' + IntToStr(Anos));

end;
``` 

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

---

# 🧠 Exercícios de Domínio - Fundamentos Delphi

Preparação para os **50 Exercícios Profissionais**

---

## 🧩 BLOCO 1: Variáveis, Constantes e Tipos (10 exercícios)

**Ex 1.1 - Declaração Básica**  
Declare variáveis de todos os tipos (`Integer`, `Double`, `String`, `Boolean`, `TDate`) e atribua valores. Exiba tudo em um `ShowMessage` formatado.  
**Componentes:** Apenas `Button`

```pascal
procedure TForm1.Bloco1;
var
    int: Integer;
  D: double;
  S: string;
  Date: TDate;
  B: boolean;
  TDate: TdateTime;
begin
  int := 20;
  D := 11.2;
  S := 'Alastor';
  B := False;
  TDate := Now;
  ShowMessage('Inteiro: ' + IntToStr(int) + sLineBreak + 'Double: '
    + FloatToStr(D) + sLineBreak + 'String: ' + S + sLineBreak + 'Boolean: ' +
    BoolToStr(B, True) + sLineBreak + 'Data/Hora: ' + DateTimeToStr(TDate));

end;
```

**Ex 1.2 - Escopo de Variáveis**  
Crie uma variável local dentro da `procedure` e outra no nível `private` da `form`. Mostre a diferença de acesso entre elas.  
**Componentes:** `Button`

```pascal
procedure TForm1.Bloco2;
var
    local: string;
begin
  local := 'Essa é local';
  global := 'Essa é Global';

  ShowMessage(local + #13#10 + global);
end;
```

**Ex 1.3 - Constantes vs Variáveis**  
Declare `PI` como constante e `raio` como variável. Calcule área do círculo. Tente mudar `PI` (vai dar erro) para demonstrar imutabilidade.  
**Componentes:** `Edit` (raio), `Button`, `Label` (resultado)

```pascal
procedure TForm1.Bloco3;
const
    pi = 3.14;
var
    raio, area: double;
begin
  raio := 3;
  area := pi * (raio * raio);

  ShowMessage('A área do círculo é: ' + FloatToStr(area));
end;

```

**Ex 1.4 - Conversões de Tipo**  
Receba um número em `Edit` (`String`), converta para `Integer`, `Double` e `Boolean` (`0=False`, outros=`True`). Exiba os 3 resultados.  
**Componentes:** `Edit`, `Button`, `Label`

```pascal
procedure TForm1.Bloco4;
var
    entrada: string;
  numInt: Integer;
  numDouble: double;
  numBool: boolean;
begin
  entrada := edt1.Text;

  if not TryStrToInt(entrada, numInt) then
  begin
    ShowMessage('Valor inválido para conversão em Integer.');
    Exit;
  end;

  if not TryStrToFloat(entrada, numDouble) then
  begin
    ShowMessage('Valor inválido para conversão em Double.');
    Exit;
  end;

  numBool := numInt <> 0;

  lbl1.Caption := 'Integer: ' + IntToStr(numInt) + sLineBreak + 'Double: ' + FloatToStr(numDouble) + sLineBreak + 'Boolean: ' +
    BoolToStr(numBool, True);

end;
```

**Ex 1.5 - Array Simples**  
Declare array de 5 strings com nomes de frutas. Percorra com `FOR` e exiba em `ShowMessage`.  
**Componentes:** `Button`
```pascal
procedure TForm1.Bloco5;         // array simples
var
    Frutas: array [0 .. 5] of String;
  lista: string;
  i: Integer;
begin

  Frutas[1] := 'Limão';
  Frutas[2] := 'Laranja';
  Frutas[3] := 'Morango';
  Frutas[4] := 'Melão';
  Frutas[5] := 'Goiaba';

  lista := '';
  for i := 1 to 5 do
    lista := lista + Frutas[i] + sLineBreak;
  ShowMessage('Frutas:' + sLineBreak + lista);

end;
```


**Ex 1.6 - Array Dinâmico**  
Peça ao usuário quantos números quer armazenar. Crie array dinâmico, preencha e exiba a soma.  
**Componentes:** `Edit` (quantidade), `Button`

**Ex 1.7 - Tipos Numéricos**  
Compare `Integer` vs `Double`: divida 10 por 3 nos dois tipos e mostre a diferença de resultado.  
**Componentes:** `Button`

**Ex 1.8 - String vs Char**  
Receba uma palavra, acesse o primeiro caractere (`Char`) e exiba tipo e valor.  
**Componentes:** `Edit`, `Button`

**Ex 1.9 - Boolean em Prática**  
Crie 3 variáveis `Boolean` (`temCNH`, `maiorIdade`, `aprovadoExame`). Use `AND` para verificar se pode dirigir.  
**Componentes:** 3 `CheckBox`, `Button`, `Label`

**Ex 1.10 - Constantes Tipadas**  
Declare constantes para: taxa de juros (`Double`), nome empresa (`String`), ano fundação (`Integer`). Use todas em um cálculo/mensagem.  
**Componentes:** `Edit`, `Button`

---

## 🔗 BLOCO 2: Operadores Lógicos e Relacionais (10 exercícios)

**Ex 2.1 - Igual e Diferente**  
Receba dois números, teste se são iguais (`=`) ou diferentes (`<>`). Exiba resultado.  
**Componentes:** 2 `Edit`, `Button`, `Label`

**Ex 2.2 - Maior e Menor**  
Compare 3 números e diga qual é o maior, qual o menor e qual está no meio.  
**Componentes:** 3 `Edit`, `Button`, `Label`

**Ex 2.3 - Operador AND**  
Valide login: usuário = `"admin"` AND senha = `"1234"`. Só aprova se ambos corretos.  
**Componentes:** 2 `Edit`, `Button`

**Ex 2.4 - Operador OR**  
Sistema de desconto: cliente VIP OR compra > R$500 ganha 10%. Teste as 3 combinações.  
**Componentes:** `Edit` (valor), `CheckBox` (VIP), `Button`

**Ex 2.5 - Operador NOT**  
Inverta uma condição: `NOT(idade < 18)` é o mesmo que `(idade >= 18)`. Demonstre.  
**Componentes:** `Edit`, `Button`

**Ex 2.6 - Combinação AND/OR**  
Aprovação de crédito: `(renda > 2000 AND score > 600) OR (temFiador)`. Teste.  
**Componentes:** 2 `Edit`, `CheckBox`, `Button`

**Ex 2.7 - Precedência de Operadores**  
Calcule: `10 + 5 * 2` vs `(10 + 5) * 2`. Mostre diferença de precedência.  
**Componentes:** `Button`

**Ex 2.8 - Comparação de Strings**  
Compare duas strings: igual, diferente, maior (alfabeticamente), menor.  
**Componentes:** 2 `Edit`, `Button`

**Ex 2.9 - Operadores com Boolean**  
Crie tabela verdade visual: `A AND B`, `A OR B`, `NOT A` para todos os casos (`True/False`).  
**Componentes:** 2 `CheckBox` (A, B), `Button`, `Memo` (resultado)

**Ex 2.10 - Condições Aninhadas**  
Classificação de idade:  
- 0-12: criança  
- 13-17: adolescente  
- 18-59: adulto  
- 60+: idoso  
**Componentes:** `Edit`, `Button`, `Label`

---

## 🧠 BLOCO 3: Estruturas de Decisão (10 exercícios)

**Ex 3.1 - IF Simples**  
Se número for par, exiba `"PAR"`, senão não faça nada.  
**Componentes:** `Edit`, `Button`

**Ex 3.2 - IF/ELSE Básico**  
Número positivo ou negativo?  
**Componentes:** `Edit`, `Button`, `Label`

**Ex 3.3 - IF/ELSE IF**  
Nota:  
- 0-4: F  
- 5-6: D  
- 7-8: C  
- 9-10: A  
**Componentes:** `Edit`, `Button`, `Label`

**Ex 3.4 - CASE Simples**  
Menu de opções:  
1 - Cadastrar  
2 - Consultar  
3 - Alterar  
4 - Excluir  
**Componentes:** `Edit` (opção), `Button`

**Ex 3.5 - CASE com Faixas**  
Classifique temperatura:  
- <0: congelante  
- 0-15: frio  
- 16-25: agradável  
- >25: quente  
**Componentes:** `Edit`, `Button`, `Label`

**Ex 3.6 - IF Aninhado**  
Calcule frete:  
- Nacional: SP = 10, outros = 20  
- Internacional: 50  
**Componentes:** 2 `RadioButton` (Nacional/Internacional), `Edit` (estado), `Button`

**Ex 3.7 - Múltiplas Condições**  
Triângulo válido: `a+b>c AND a+c>b AND b+c>a`  
**Componentes:** 3 `Edit`, `Button`

**Ex 3.8 - Short-Circuit**  
Demonstre short-circuit: `IF (x <> 0) AND (10/x > 5)`  
**Componentes:** `Edit`, `Button`

**Ex 3.9 - Validações Sequenciais**  
Valide formulário:  
- Nome não vazio  
- Idade entre 18-100  
- Email com `@`  
Use `IFs` sequenciais com `Exit`.  
**Componentes:** 3 `Edit`, `Button`

**Ex 3.10 - Decisão Complexa**  
Calcule imposto progressivo:  
- até 2k: isento  
- 2-4k: 7.5%  
- 4-6k: 15%  
- 6k+: 22.5%  
**Componentes:** `Edit`, `Button`, `Label`

---

## 🔁 BLOCO 4: Loops (10 exercícios)

**Ex 4.1 - FOR Crescente**  
Conte de 1 a 10 e exiba cada número.  
**Componentes:** `Button`, `Memo`

**Ex 4.2 - FOR Decrescente**  
Contagem regressiva: 10 até 0 usando `DOWNTO`.  
**Componentes:** `Button`, `Memo`

**Ex 4.3 - WHILE com Condição**  
Some números digitados até o usuário digitar 0.  
**Componentes:** `Edit`, `Button`, `Label` (soma)

**Ex 4.4 - REPEAT-UNTIL**  
Peça senha até acertar `"123"`. Mínimo 1 tentativa garantida.  
**Componentes:** `Edit`, `Button`

**Ex 4.5 - Break em FOR**  
Procure número 7 em array de 1 a 10. Use `Break` ao encontrar.  
**Componentes:** `Button`

**Ex 4.6 - Continue em FOR**  
Exiba apenas ímpares de 1 a 20 usando `Continue`.  
**Componentes:** `Button`, `Memo`

**Ex 4.7 - Loop Aninhado
