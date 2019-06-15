{*******************************************************}
{                                                       }
{ Borland Delphi Visual Component Library               }
{                SOAP Support                           }
{                                                       }
{ Copyright (c) 2001 Borland Software Corporation       }
{                                                       }
{*******************************************************}

unit SOAPEnv;

interface

uses Classes, SysUtils, xmldom, XMLDoc, XMLIntf, OPConvert;

type
  TSoapEnvelope = class
  public
    function MakeEnvelope(Doc: IXMLDocument; Options: TJSONConvertOptions): IXMLNode;
    function MakeHeader(ParentNode: IXMLNode): IXMLNode;
    function MakeBody(ParentNode: IXMLNOde): IXMLNode;
    function MakeFault(ParentNode: IXMLNOde): IXMLNode;
  end;

implementation

uses  RESTJSONConst;

function TSoapEnvelope.MakeEnvelope(Doc: IXMLDocument; Options: TJSONConvertOptions): IXMLNode;
begin
  Result := Doc.CreateNode(SSoapEnvelope);
  Result.DeclareNamespace(SXMLSchemaNameSpacePre, XMLSchemaNameSpace);
  Result.DeclareNamespace(SXMLSchemaInstNameSpace99Pre, XMLSchemaInstNameSpace);
  Doc.DocumentElement := Result;
end;


function TSoapEnvelope.MakeBody(ParentNode: IXMLNode): IXMLNode;
begin
end;

function TSoapEnvelope.MakeHeader(ParentNode: IXMLNode): IXMLNode;
begin
end;

function TSoapEnvelope.MakeFault(ParentNode: IXMLNode): IXMLNode;
begin
end;

end.
