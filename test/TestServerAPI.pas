unit TestServerAPI;

interface

uses
  Classes,
  WebNode, IntfInfo, InvokeRegistry,
  RESTJSONHTTPClient, RESTJSONHTTPTrans, OPToRESTJSONConv,
  TestAPIIntf,
  superobject;

type
  TTestRESTServerAPI = class(THTTPRio)
  protected
    function CreateDefaultConverter: TOPToJSONConvert; override;
  public
    procedure SetWebNode(const AWebNode: IWebNode);
  end;

implementation

type
  TTestJSONConvert = class(TOPToJSONConvert)
  protected
    function ResponseValid(const JSONDoc: ISuperObject;
                           const IntfMD: TIntfMetaData;
                           const MD: TIntfMethEntry;
                           Context: TInvContext): Boolean; override;
    function ResponseIsError(JSONDoc: ISuperObject; out ErrorNode: ISuperObject): Boolean; override;
  end;
  
{ TTestRESTServerAPI }

function TTestRESTServerAPI.CreateDefaultConverter: TOPToJSONConvert;
begin
  Result := TTestJSONConvert.Create(Self);
end;

procedure TTestRESTServerAPI.SetWebNode(const AWebNode: IWebNode);
begin
  WebNode := AWebNode;
end;

{ TTestJSONConvert }

function TTestJSONConvert.ResponseIsError(JSONDoc: ISuperObject;
  out ErrorNode: ISuperObject): Boolean;
begin
  Result := JSONDoc.S['Error'] <> '';
  if Result then
    ErrorNode := JSONDoc.O['Error']
  else
    ErrorNode := nil;
end;

function TTestJSONConvert.ResponseValid(const JSONDoc: ISuperObject; const
    IntfMD: TIntfMetaData; const MD: TIntfMethEntry; Context: TInvContext):
    Boolean;
begin
  Result := inherited ResponseValid(JSONDoc, IntfMD, MD, Context);
end;

end.
