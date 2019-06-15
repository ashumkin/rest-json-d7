{*******************************************************}
{                                                       }
{ Borland Delphi Visual Component Library               }
{                SOAP Support                           }
{                                                       }
{ Copyright (c) 2001 Borland Software Corporation       }
{                                                       }
{*******************************************************}

unit JSONConv;

interface

uses Classes, xmldom, XMLIntf, XMLDoc, OPConvert;

type

  IDOMHeaderProcessor = interface
  ['{27F23F8F-23A2-4257-95A8-0204EEFF937B}']
    procedure ProcessHeader(HeaderNode: IXMLNode; var Handled, AbortRequest: Boolean);
    function  CreateHeader(HeaderNode: IXMLNode): IXMLNode;
  end;

  TDOMHeaderProcessorEntry = record
    Processor: IDOMHeaderProcessor;
    NameSpace: WideString;
    HeaderName: WideString;
    TypeName: WideString;
  end;

  TDOMHeaderProcessorArray = array of TDOMHeaderProcessorEntry;

  TJSONProcessor = class(TComponent, IInterface)
  private
    FRefCount: Integer;
    FOwnerIsComponent: Boolean;
  protected
    FHeaderProcessors:  TDOMHeaderProcessorArray;
    function  FindHeaderProcessor(Namespace, HeaderName, TypeName: WideString): IDOMHeaderProcessor; virtual;
    function  MakeHeaderNodes(HeaderNode: IXMLNode): IXMLNode;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AddHeaderProcessor(Namespace, HeaderName, TypeName: WideString; Processor: IDOMHeaderProcessor); virtual;
    procedure DefaultProcessHeader(HeaderNode: IXMLNode; var Handled, AbortRequest: Boolean); virtual;

    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses Variants, SysUtils, RESTJSONConst, InvokeRegistry, {$IFDEF MSWINDOWS}Windows{$ENDIF}{$IFDEF LINUX}Libc{$ENDIF};

{ TJSONProcessor }

destructor TJSONProcessor.Destroy;
begin
  inherited;
end;

class function TJSONProcessor.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TJSONProcessor(Result).FRefCount := 1;
end;

procedure TJSONProcessor.AfterConstruction;
begin
  inherited;
  FOwnerIsComponent := Assigned(Owner) and (Owner is TComponent);
  InterlockedDecrement(FRefCount);
end;

{ IInterface }

function TJSONProcessor._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount)
end;

function TJSONProcessor._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  { If we are not being used as a TComponent, then use refcount to manage our
    lifetime as with TInterfacedObject. }
  if (Result = 0) and not FOwnerIsComponent then
    Destroy;
end;

procedure TJSONProcessor.AddHeaderProcessor(Namespace, HeaderName, TypeName: WideString;
  Processor: IDOMHeaderProcessor);
var
  I: Integer;
begin
  I := Length(FHeaderProcessors);
  SetLength(FHeaderProcessors, I + 1);
  FHeaderProcessors[I].NameSpace := NameSpace;
  FHeaderProcessors[I].HeaderName := HeaderName;
  FHeaderProcessors[I].TypeName := TypeName;
  FHeaderProcessors[I].Processor := Processor;
end;

procedure TJSONProcessor.DefaultProcessHeader(HeaderNode: IXMLNode;
  var Handled, AbortRequest: Boolean);
begin
end;

function TJSONProcessor.FindHeaderProcessor(Namespace, HeaderName,
  TypeName: WideString): IDOMHeaderProcessor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FHeaderProcessors) - 1 do
  begin
    if (FHeaderProcessors[I].NameSpace = NameSpace) and
       (FHeaderProcessors[I].HeaderName = HeaderName) and
       (FHeaderProcessors[I].TypeName = TypeName) then
    begin
      Result :=  FHeaderProcessors[I].Processor;
      Exit;
    end;
  end;
end;

function TJSONProcessor.MakeHeaderNodes(HeaderNode: IXMLNode): IXMLNode;
var
  I: Integer;
  Node: IXMLNode;
begin
  for I := 0 to Length(FHeaderProcessors) - 1 do
    if Assigned(FHeaderProcessors[I].Processor) then
    begin
      Node := FHeaderProcessors[I].Processor.CreateHeader(HeaderNode);
      if Node <> nil then
        HeaderNode.ChildNodes.Add(Node);
    end;
end;

end.
