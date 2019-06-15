unit TestAPI;

interface

uses
  Classes, WebNode, IntfInfo,
  TestFramework,
  TestAPIIntf;

type
  TMockWebNode = class(TComponent, IInterface, IWebNode)
  private
    FResult: TStringStream;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure BeforeExecute(const IntfMD: TIntfMetaData;
                            const MethMD: TIntfMethEntry;
                            MethodIndex: Integer);

    procedure Execute(const Request: TStream; Response: TStream); overload;

    function WillReturn(const AJSON: string): TMockWebNode;
  end;

  TTestAPI = class(TTestCase)
  private
    FMockWebNode: TMockWebNode;
    function GetRESTServerAPI(AMockWebNode: IWebNode): ITestRESTServerAPI;
  public
    procedure SetUp; override;
  published
    procedure Test_WhenSimpleMethod_WithComplexResult_ShouldSucceed;
    procedure TestGetServerData_Error_String;
    procedure Test_Error_ComplexObject_ShouldRaise;

    procedure Test_SimpleRequest_SimpleResponse_ShouldSucceed;

    procedure Test_ArrayOfObjects_NotEmpty;
  end;

implementation

uses
  SysUtils,
  TestServerAPI;

{ TMockWebNode }

procedure TMockWebNode.AfterConstruction;
begin
  inherited;
  FResult := TStringStream.Create('');
end;

procedure TMockWebNode.BeforeDestruction;
begin
  FreeAndNil(FResult);
  inherited;
end;

procedure TMockWebNode.BeforeExecute(const IntfMD: TIntfMetaData;
  const MethMD: TIntfMethEntry; MethodIndex: Integer);
begin
  // do nothing
end;

procedure TMockWebNode.Execute(const Request: TStream; Response: TStream);
begin
  Response.Position := 0;
  FResult.Position := 0;
  Response.CopyFrom(FResult, 0);
end;

function TMockWebNode.WillReturn(const AJSON: string): TMockWebNode;
begin
  FResult.Position := 0;
  FResult.WriteString(UTF8Encode(AJSON));
  Result:= Self;
end;

{ TTestAPI }

function TTestAPI.GetRESTServerAPI(AMockWebNode: IWebNode): ITestRESTServerAPI;
var
  LKKMServerAPI: TTestRESTServerAPI;
begin
  LKKMServerAPI := TTestRESTServerAPI.Create(nil);
  if Assigned(AMockWebNode) then
    LKKMServerAPI.SetWebNode(AMockWebNode);
  Result := LKKMServerAPI as ITestRESTServerAPI;
end;

procedure TTestAPI.SetUp;
begin
  inherited;
  FMockWebNode := TMockWebNode.Create(nil);
end;

procedure TTestAPI.Test_Error_ComplexObject_ShouldRaise;
begin
  FMockWebNode.WillReturn(
    '{' + #13#10 +
    '  "Command": "GetServerData",' + #13#10 +
    '  "Error": { "code": 12345, "message": "GetServerData command error" },' + #13#10 +
    '  "Warning": "",' + #13#10 +
    '  "Status": 2,' + #13#10 +
    '  "IdCommand": ""' + #13#10 +
    '}');
  StartExpectingException(ETestServerCommandError);
  try
    GetRESTServerAPI(FMockWebNode).GetServerData;
  except
    on E: ETestServerCommandError do
    begin
      CheckEquals('GetServerData command error', E.Message);
      raise;
    end;
  end;
  StopExpectingException('There should be an error');
end;

procedure TTestAPI.TestGetServerData_Error_String;
begin
  FMockWebNode.WillReturn(
    '{' + #13#10 +
    '  "Command": "GetServerData",' + #13#10 +
    '  "Error": "GetServerData command error",' + #13#10 +
    '  "Warning": "",' + #13#10 +
    '  "Status": 2,' + #13#10 +
    '  "IdCommand": ""' + #13#10 +
    '}');
  StartExpectingException(ETestServerCommandError);
  try
    GetRESTServerAPI(FMockWebNode).GetServerData;
  except
    on E: ETestServerCommandError do
    begin
      CheckEquals('GetServerData command error', E.Message);
      raise;
    end;
  end;
  StopExpectingException('There should be an error');
end;

procedure TTestAPI.Test_WhenSimpleMethod_WithComplexResult_ShouldSucceed;
var
  LServerData: TTestServerDataResponse;
begin
  FMockWebNode.WillReturn(
    '{' + #13#10 +
    '  "ServerData": {' + #13#10 +
    '    "Integer": 12345,' + #13#10 +
    '    "PropDateTime": "2019-06-17T12:34:56",' + #13#10 +
    '  },' + #13#10 +
    '  "PropWideString": "Got wide string",' + #13#10 +
    '  "Error": "",' + #13#10 +
    '  "Enumerable": 1,' + #13#10 +
    '}');
  LServerData := GetRESTServerAPI(FMockWebNode).GetServerData;
  CheckEquals('Got wide string', LServerData.PropWideString);
  CheckEquals(Ord(tesError), Ord(LServerData.PropEnumerable), 'PropEnumerable');
  CheckEquals(12345, LServerData.ServerData.PropInteger, 'PropInteger');
  CheckEquals('2019-06-17T12:34:56', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', LServerData.ServerData.PropDateTime), 'PropDateTime');
end;

procedure TTestAPI.Test_ArrayOfObjects_NotEmpty;
var
  LGetArrayInpuTFilter: TTestGetArrayInputFilter;
  LGetArrayResponse: TTestGetArrayResponse;
begin
  FMockWebNode.WillReturn(
    '{' + #13#10 +
    '    "ListUnit": [' + #13#10 +
    '        {' + #13#10 +
    '            "NumDevice": 1,' + #13#10 +
    '            "NameDevice": "KKT Эмулятор",' + #13#10 +
    '            "IdDevice": "71AC1A45-FDD3-4E5B-9F41-BF065F154E3F",' + #13#10 +
    '        },' + #13#10 +
    '        {' + #13#10 +
    '            "NumDevice": 2,' + #13#10 +
    '            "NameDevice": "Эмулятор эк.терминала",' + #13#10 +
    '            "IdDevice": "426c3b49-5747-43bc-90a1-ae03e7933675",' + #13#10 +
    '        },' + #13#10 +
    '        {' + #13#10 +
    '            "NumDevice": 3,' + #13#10 +
    '            "NameDevice": "KKT Эмулятор",' + #13#10 +
    '            "IdDevice": "c0b590bb-16fa-43ee-9cb4-bbce7a3a335e",' + #13#10 +
    '        }' + #13#10 +
    '    ],' + #13#10 +
    '    "Command": "List",' + #13#10 +
    '    "Error": "",' + #13#10 +
    '    "Warning": "",' + #13#10 +
    '    "Status": 0,' + #13#10 +
    '    "IdCommand": ""' + #13#10 +
    '}'
  );
  LGetArrayInpuTFilter := TTestGetArrayInputFilter.Create;
  LGetArrayResponse := GetRESTServerAPI(FMockWebNode).GetArray(LGetArrayInpuTFilter);
  CheckEquals(3, Length(LGetArrayResponse.ArrayItems), 'Count');

  CheckEquals(1, LGetArrayResponse.ArrayItems[0].NumDevice, 'NumDevice');
  CheckEquals('KKT Эмулятор', LGetArrayResponse.ArrayItems[0].NameDevice, 'NameDevice');
  CheckEquals('71AC1A45-FDD3-4E5B-9F41-BF065F154E3F', LGetArrayResponse.ArrayItems[0].IdDevice, 'IdDevice');

  CheckEquals(2, LGetArrayResponse.ArrayItems[1].NumDevice, 'NumDevice');
  CheckEquals('Эмулятор эк.терминала', LGetArrayResponse.ArrayItems[1].NameDevice, 'NameDevice');
  CheckEquals('426c3b49-5747-43bc-90a1-ae03e7933675', LGetArrayResponse.ArrayItems[1].IdDevice, 'IdDevice');

  CheckEquals(3, LGetArrayResponse.ArrayItems[2].NumDevice, 'NumDevice');
  CheckEquals('KKT Эмулятор', LGetArrayResponse.ArrayItems[2].NameDevice, 'NameDevice');
  CheckEquals('c0b590bb-16fa-43ee-9cb4-bbce7a3a335e', LGetArrayResponse.ArrayItems[2].IdDevice, 'IdDevice');
end;

procedure TTestAPI.Test_SimpleRequest_SimpleResponse_ShouldSucceed;
var
  LTestServerSimpleResponse: TTestServerSimpleResponse;
begin
  FMockWebNode.WillReturn('{"FloatNumber": 25.01, "PropWideString": "anything","Error": ""}');
  LTestServerSimpleResponse:= GetRESTServerAPI(FMockWebNode).GetSimple;
  CheckEquals(25.01, LTestServerSimpleResponse.Extended, 0.0001);
  CheckEquals('anything', LTestServerSimpleResponse.PropWideString);  
end;

initialization
  RegisterTest(TTestAPI.Suite);

end.
