# 📄 XML no [Sol.NET](http://sol.net/): Guia Completo

## 🎯 O que é XML?

**XML** (eXtensible Markup Language) é uma linguagem de marcação que define regras para codificação de documentos em formato legível tanto para humanos quanto para máquinas. É amplamente utilizado para:

- **Troca de dados** entre sistemas diferentes
- **Armazenamento estruturado** de configurações
- **Documentos fiscais** (NF-e, NFC-e, CT-e, MDF-e)
- **Comunicação** com Web Services

### Estrutura Básica do XML

```xml
<?xml version="1.0" encoding="UTF-8"?>
<raiz>
  <elemento atributo="valor">
    <subelemento>Conteúdo</subelemento>
  </elemento>
</raiz>

```

**Características principais:**

- **Tags** em pares: `<tag>conteúdo</tag>`
- **Hierarquia** bem definida (árvore)
- **Atributos** opcionais: `<tag atributo="valor">`
- **Case-sensitive**: `<Nome>` é diferente de `<nome>`
- **Auto-descritivo**: os nomes das tags descrevem o conteúdo

---

## 🏢 Uso do XML no [Sol.NET](http://sol.net/)

No contexto do [Sol.NET](http://sol.net/) ERP, o XML é utilizado principalmente para:

### 1. **Documentos Fiscais Eletrônicos**

O [Sol.NET](http://sol.net/) trabalha extensivamente com XMLs de documentos fiscais:

### NF-e (Nota Fiscal Eletrônica)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<nfeProc versao="4.00">
  <NFe>
    <infNFe Id="NFe35240512345678000199550010000001234567890123">
      <ide>
        <cUF>35</cUF>
        <natOp>VENDA</natOp>
        <mod>55</mod>
        <serie>1</serie>
        <nNF>123456</nNF>
        <dhEmi>2024-05-01T10:30:00-03:00</dhEmi>
      </ide>
      <emit>
        <CNPJ>12345678000199</CNPJ>
        <xNome>EMPRESA EXEMPLO LTDA</xNome>
        <enderEmit>
          <xLgr>RUA EXEMPLO</xLgr>
          <nro>100</nro>
          <xBairro>CENTRO</xBairro>
          <cMun>3550308</cMun>
          <xMun>SAO PAULO</xMun>
          <UF>SP</UF>
          <CEP>01000000</CEP>
        </enderEmit>
      </emit>
      <dest>
        <!-- Dados do destinatário -->
      </dest>
      <det nItem="1">
        <prod>
          <cProd>001</cProd>
          <xProd>PRODUTO EXEMPLO</xProd>
          <NCM>12345678</NCM>
          <CFOP>5102</CFOP>
          <uCom>UN</uCom>
          <qCom>1.0000</qCom>
          <vUnCom>100.00</vUnCom>
          <vProd>100.00</vProd>
        </prod>
        <imposto>
          <!-- Impostos -->
        </imposto>
      </det>
      <total>
        <!-- Totais -->
      </total>
    </infNFe>
  </NFe>
  <protNFe>
    <!-- Protocolo de autorização -->
  </protNFe>
</nfeProc>

```

**Processos que utilizam XML de NF-e:**

- **Emissão:** Geração do XML para envio à SEFAZ
- **Recepção:** Importação de XMLs de compras (manifestação)
- **Armazenamento:** Backup dos XMLs autorizados
- **Consulta:** Validação de notas de terceiros

### NFC-e (Nota Fiscal do Consumidor Eletrônica)

Similar à NF-e, mas com modelo `65` e campos específicos para varejo.

**No Sol.NET_MonitorNFCe:**

```
// Exemplo conceitual de processamento
procedure ProcessarNFCePendente;
var
  XMLNFCe: string;
  XMLRetorno: IXMLDocument;
begin
  // 1. Monta XML da NFC-e
  XMLNFCe := GerarXMLNFCe(IdMovimento);

  // 2. Assina digitalmente
  XMLNFCe := AssinarXML(XMLNFCe, CertificadoDigital);

  // 3. Envia para SEFAZ
  XMLRetorno := EnviarParaSEFAZ(XMLNFCe);

  // 4. Processa retorno
  if StatusAutorizacao(XMLRetorno) = 100 then
    SalvarProtocolo(XMLRetorno);
end;

```

---

### 2. **Integrações com Plataformas (Framework de Integrações)**

### iFood

A integração com o iFood utiliza JSON (similar conceitual ao XML), mas o framework pode converter entre formatos:

```
// Exemplo conceitual
type
  TIntegracaoIFood = class
    function ReceberPedido(JSONPedido: string): TPedido;
    function EnviarProduto(Produto: TProduto): Boolean;
  end;

```

### Scanntech (Clube de Promoções)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<promocoes>
  <promocao id="12345">
    <titulo>Promoção de Verão</titulo>
    <tipo>desconto_percentual</tipo>
    <dtInicio>2024-01-01</dtInicio>
    <dtFinal>2024-01-31</dtFinal>
    <produtos>
      <produto codigo="001" preco="89.90"/>
      <produto codigo="002" preco="129.90"/>
    </produtos>
  </promocao>
</promocoes>

```

**Processamento no [Sol.NET](http://sol.net/):**

```
procedure TIntegracaoScanntech.BaixarPromocoes;
var
  XMLPromocoes: IXMLDocument;
  NodePromocao: IXMLNode;
begin
  // 1. Baixar XML da API
  XMLPromocoes := DownloadXMLPromocoes;

  // 2. Percorrer nós
  NodePromocao := XMLPromocoes.DocumentElement.ChildNodes.First;
  while Assigned(NodePromocao) do
  begin
    ProcessarPromocao(NodePromocao);
    NodePromocao := NodePromocao.NextSibling;
  end;
end;

```

---

### 3. **Integrações Fiscais (Framework Fiscal)**

As integrações com provedores fiscais (Schmitt, IMendes, MixFiscal) utilizam XML para:

### Consulta de NCM/Tributação

```xml
<!-- Requisição -->
<consultaNCM>
  <ncm>12345678</ncm>
  <ufOrigem>SP</ufOrigem>
  <ufDestino>RJ</ufDestino>
  <cfop>5102</cfop>
</consultaNCM>

<!-- Resposta -->
<retornoNCM>
  <produto>
    <ncm>12345678</ncm>
    <descricao>PRODUTO EXEMPLO</descricao>
    <cest>1234567</cest>
    <tributacaoEstadual>
      <icms>
        <cst>00</cst>
        <aliquota>18.00</aliquota>
      </icms>
    </tributacaoEstadual>
    <tributacaoFederal>
      <pis>
        <cst>01</cst>
        <aliquota>1.65</aliquota>
      </pis>
      <cofins>
        <cst>01</cst>
        <aliquota>7.60</aliquota>
      </cofins>
    </tributacaoFederal>
  </produto>
</retornoNCM>

```

**Implementação no [Sol.NET](http://sol.net/):**

```
function TFiscalSchmitt.ConsultarNCM(NCM: string; UFOrigem, UFDestino: string): TProdutoNCM;
var
  XMLRequisicao: IXMLDocument;
  XMLResposta: IXMLDocument;
begin
  // Monta XML de requisição
  XMLRequisicao := CriarXMLConsulta(NCM, UFOrigem, UFDestino);

  // Envia para Web Service
  XMLResposta := EnviarWebService(XMLRequisicao);

  // Parseia resposta
  Result := ParsearRetornoNCM(XMLResposta);
end;

```

---

### 4. **Configurações e Parametrizações**

Algumas configurações podem ser armazenadas em XML:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuracoes>
  <integracao nome="iFood" ativo="true">
    <credenciais>
      <clientId>abc123</clientId>
      <merchantId>xyz789</merchantId>
    </credenciais>
    <opcoes>
      <sincronizarEstoque>true</sincronizarEstoque>
      <intervaloMinutos>15</intervaloMinutos>
    </opcoes>
  </integracao>

  <integracao nome="Scanntech" ativo="false">
    <!-- Configurações específicas -->
  </integracao>
</configuracoes>

```

---

## 🔧 Manipulação de XML em Delphi

### Principais Componentes

### 1. **IXMLDocument (MSXML)**

```
uses Xml.XMLDoc, Xml.XMLIntf;

procedure ExemploLeituraXML;
var
  XMLDoc: IXMLDocument;
  Node: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromFile('C:\\exemplo.xml');
    XMLDoc.Active := True;

    // Navegar pela estrutura
    Node := XMLDoc.DocumentElement.ChildNodes['elemento'];
    ShowMessage(Node.Text);

    // Ler atributo
    var Atributo := Node.Attributes['nome'];
  finally
    XMLDoc := nil;
  end;
end;

```

### 2. **XPath (Consultas)**

```
// Buscar elemento específico
var Node: IXMLNode := XMLDoc.DocumentElement.SelectSingleNode('//produto[@codigo="001"]');

// Buscar múltiplos elementos
var NodeList: IXMLNodeList := XMLDoc.DocumentElement.SelectNodes('//produtos/produto');

```

### 3. **Criação de XML**

```
function CriarXMLProduto(Produto: TProduto): string;
var
  XMLDoc: IXMLDocument;
  RootNode, ProdNode: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := True;

  // Criar nó raiz
  RootNode := XMLDoc.AddChild('produtos');

  // Adicionar produto
  ProdNode := RootNode.AddChild('produto');
  ProdNode.Attributes['codigo'] := Produto.Codigo;
  ProdNode.AddChild('descricao').Text := Produto.Descricao;
  ProdNode.AddChild('preco').Text := FloatToStr(Produto.Preco);

  // Retornar como string
  Result := XMLDoc.XML.Text;
end;

```

---

## 📊 XML vs JSON no [Sol.NET](http://sol.net/)

| Aspecto | XML | JSON | Uso no [Sol.NET](http://sol.net/) |
| --- | --- | --- | --- |
| **Documentos Fiscais** | ✅ Obrigatório (SEFAZ) | ❌ Não suportado | NF-e, NFC-e, CT-e, MDF-e |
| **APIs Modernas** | ⚠️ Suporte legado | ✅ Preferido | iFood, WBuy, HetoBank |
| **Configurações** | ✅ Uso tradicional | ✅ Mais leve | Ambos possíveis |
| **Tamanho** | ❌ Maior (tags duplicadas) | ✅ Menor | - |
| **Legibilidade** | ✅ Auto-descritivo | ✅ Mais conciso | - |
| **Validação** | ✅ XSD (Schema) | ⚠️ JSON Schema | Fiscal usa XSD |

---

## 🔐 Validação e Assinatura Digital (Fiscal)

### Validação por Schema (XSD)

```
procedure ValidarXMLNFe(XMLNFe: string);
var
  XMLDoc: IXMLDocument;
  SchemaCollection: IXMLSchemaCollection2;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.LoadFromXML(XMLNFe);

  SchemaCollection := CoXMLSchemaCache60.Create;
  SchemaCollection.Add('<http://www.portalfiscal.inf.br/nfe>',
    'C:\\Schemas\\nfe_v4.00.xsd');

  XMLDoc.DOMDocument.schemas := SchemaCollection;

  // Se houver erro, levanta exceção
  var Erro := XMLDoc.DOMDocument.validate;
  if Assigned(Erro) then
    raise Exception.Create('XML inválido: ' + Erro.reason);
end;

```

### Assinatura Digital

```
function AssinarXMLNFe(XMLNFe: string; Certificado: TCertificado): string;
var
  XMLDoc: IXMLDocument;
  Assinador: TAssinadorXML;
begin
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.LoadFromXML(XMLNFe);

  Assinador := TAssinadorXML.Create;
  try
    Assinador.Certificado := Certificado;
    Assinador.AssinarNFe(XMLDoc);
    Result := XMLDoc.XML.Text;
  finally
    Assinador.Free;
  end;
end;

```

---

## 🎯 Boas Práticas no [Sol.NET](http://sol.net/)

### 1. **Sempre Validar XML de Entrada**

```
procedure ProcessarXMLExterno(XMLString: string);
begin
  if not ValidarEstrutura(XMLString) then
    raise EExcecaoUsuario.Create('XML inválido recebido');

  // Processar...
end;

```

### 2. **Encoding Correto**

```
// Sempre especificar UTF-8 para documentos fiscais
XMLDoc.Encoding := 'UTF-8';
XMLDoc.Options := [doNodeAutoIndent];

```

### 3. **Tratamento de Erros**

```
try
  XMLDoc.LoadFromFile(Arquivo);
except
  on E: Exception do
  begin
    LogAdd('Erro ao carregar XML: ' + E.Message);
    raise EExcecaoDesenvolvedor.Create('XML malformado');
  end;
end;

```

### 4. **Namespace em Documentos Fiscais**

```
// Registrar namespace da NF-e
XMLDoc.DocumentElement.DeclareNamespace('',
  '<http://www.portalfiscal.inf.br/nfe>');

```

### 5. **Armazenamento Seguro**

```
// XMLs fiscais devem ser guardados por 5 anos (legislação)
procedure ArmazenarXMLNFe(ChaveNFe: string; XMLConteudo: string);
var
  Pasta: string;
begin
  Pasta := FormatDateTime('yyyy\\mm', Now);
  ForceDirectories('C:\\XMLs\\NFe\\' + Pasta);

  var Arquivo := 'C:\\XMLs\\NFe\\' + Pasta + '\\' + ChaveNFe + '.xml';
  TFile.WriteAllText(Arquivo, XMLConteudo, TEncoding.UTF8);
end;

```

---

## 📚 Resumo Prático

### Quando Usar XML no [Sol.NET](http://sol.net/):

✅ **Documentos Fiscais** (NF-e, NFC-e, CT-e, MDF-e)

✅ **Web Services SOAP** (integrações legadas)

✅ **Importação/Exportação** de dados estruturados

✅ **Armazenamento** de configurações complexas

✅ **Comunicação SEFAZ** (obrigatório por lei)

### Ferramentas Essenciais:

- **XMLSpy / Oxygen XML Editor**: Edição e validação
- **Notepad++** com plugin XML Tools: Visualização rápida
- **ValidadorNFe**: Validação específica de documentos fiscais
- **ACBrMonitor**: Teste de geração/envio de XMLs fiscais

---

**Documentação relacionada:**

- [Framework de Integrações](https://www.notion.so/Framework%20Integracoes.md)
- Documentação Fiscal (a ser criada)
- Padrões de Web Services (a ser criada)

*Este documento cobre os principais usos de XML no ecossistema [Sol.NET](http://sol.net/), focando em aplicações práticas e padrões de desenvolvimento.*
