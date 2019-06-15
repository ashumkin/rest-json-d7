{*******************************************************}
{                                                       }
{ Borland Delphi Visual Component Library               }
{                SOAP Support                           }
{                                                       }
{ Copyright (c) 2001 Borland Software Corporation       }
{                                                       }
{*******************************************************}

unit RESTJSONHTTPClient;

interface

uses Classes, Rio, OPConvert, OPToRESTJSONConv, RESTJSONHTTPTrans, WebNode;

type
  THTTPRIO = class(TRIO)
  private
    FDOMConverter:  TOPToJSONConvert;
    FHTTPWebNode: THTTPReqResp;
    FDefaultConverter: TOPToJSONConvert;
    FDefaultWebNode: THTTPReqResp;
    procedure SetURL(Value: string);

    function  GetDomConverter: TOPToJSONConvert;
    procedure SetDomConverter(Value: TOPToJSONConvert);

    function  GetHTTPWebNode: THTTPReqResp;
    procedure SetHTTPWebNode(Value: THTTPReqResp);

    function  GetURL: string;
  protected
    function CreateDefaultWebNode: THTTPReqResp; virtual;
    function GetDefaultWebNode: THTTPReqResp; virtual;
    function CreateDefaultConverter: TOPToJSONConvert; virtual;
    function GetDefaultConverter: TOPToJSONConvert; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property URL: string read GetURL write SetURL;
    property HTTPWebNode: THTTPReqResp read GetHTTPWebNode write SetHTTPWebNode;
    property Converter: TOPToJSONConvert read GetDomConverter write SetDOMConverter;
  end;

implementation

uses SysUtils, InvokeRegistry;

constructor THTTPRIO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Converter }
  FDomConverter := GetDefaultConverter;
  FConverter := FDomConverter as IOPConvert;
  { WebNode }
  FHTTPWebNode := GetDefaultWebNode;
  FWebNode := FHTTPWebNode as IWebNode;
end;

destructor THTTPRIO.Destroy;
begin
  if Assigned(FConverter) then
    FConverter := nil;
  if Assigned(FWebNode) then
    FWebNode := nil;

  { All components we own are automatically cleaned up }
  inherited;
end;

function THTTPRIO.GetDefaultWebNode: THTTPReqResp;
begin
  if (FDefaultWebNode = nil) then
  begin
    FDefaultWebNode := CreateDefaultWebNode;
    FDefaultWebNode.Name := 'HTTPWebNode1';                { do not localize }
    FDefaultWebNode.SetSubComponent(True);
  end;
  Result := FDefaultWebNode;
end;

function THTTPRIO.GetDefaultConverter: TOPToJSONConvert;
begin
  if (FDefaultConverter = nil) then
  begin
    FDefaultConverter := CreateDefaultConverter;
    FDefaultConverter.Name := 'Converter1';                 { do not localize }
    FDefaultConverter.SetSubComponent(True);
  end;
  Result := FDefaultConverter;
end;

procedure THTTPRIO.SetURL(Value: string);
begin
  if Assigned(FHTTPWebNode) then
  begin
    FHTTPWebNode.URL := Value;
  end;
end;

function THTTPRIO.GetDomConverter: TOPToJSONConvert;
begin
  if not Assigned(FDomConverter) then
  begin
    FDomConverter := GetDefaultConverter;
    FConverter := FDomConverter as IOPConvert;
  end;
  Result := FDomConverter;
end;

procedure THTTPRIO.SetDomConverter(Value: TOPToJSONConvert);
begin
  if Assigned(FDOMConverter) and (FDomConverter.Owner = Self) then
  begin
    FConverter := nil;
    if FDomConverter <> FDefaultConverter then
      FDomConverter.Free;
  end;
  FDomConverter := Value;
  if Value <> nil then
  begin
    FConverter := Value;
    FDomConverter.FreeNotification(Self);
  end;
end;

function THTTPRIO.GetHTTPWebNode: THTTPReqResp;
begin
  if not Assigned(FHTTPWebNode) then
  begin
    FHTTPWebNode := GetDefaultWebNode;
    FWebNode := FHTTPWebNode as IWebNode;
  end;
  Result := FHTTPWebNode;
end;

procedure THTTPRIO.SetHTTPWebNode(Value: THTTPReqResp);
var
  URL: string;
begin
  if Assigned(FHTTPWebNode) then
  begin
    { Save previous endpoint configuration }
    URL := FHTTPWebNode.URL;
    { Cleanup if we're owner and it's not out default one }
    if (FHTTPWebNode.Owner = Self) and (FHTTPWebNode <> FDefaultWebNode) then
    begin
      FWebNode := nil;
      FHTTPWebNode.Free;
    end
  end
  else
  begin
    URL := '';
  end;

  FHTTPWebNode := Value;

  if Value <> nil then
  begin
    FWebNode := Value;
    { Make sure we get notified so we may cleanup properly }
    FHTTPWebNode.FreeNotification(Self);
  end
  else
  begin
    FHTTPWebNode := FDefaultWebNode;
    FWebNode := FHTTPWebNode as IWebNode;
  end;

  { Transfer previous endpoint configuration }
  if FHTTPWebNode <> nil then
  begin
    if (URL <> '') and (FHTTPWebNode.URL = '') then
      FHTTPWebNode.URL := URL;
  end;
end;

function THTTPRIO.GetURL: string;
begin
  if Assigned(FHTTPWebNode) then
    Result := FHTTPWebNode.URL
  else
    Result := '';
end;

procedure THTTPRIO.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FHTTPWebNode) then
  begin
    FWebNode := nil;
    FHTTPWebNode := nil;
  end;
  if (Operation = opRemove) and (AComponent = FDomConverter) then
  begin
    FConverter := nil;
    FDomConverter := nil;
  end;
end;

function THTTPRIO.CreateDefaultConverter: TOPToJSONConvert;
begin
  Result := TOPToJSONConvert.Create(Self);
end;

function THTTPRIO.CreateDefaultWebNode: THTTPReqResp;
begin
  Result := THTTPReqResp.Create(Self);
end;

end.
