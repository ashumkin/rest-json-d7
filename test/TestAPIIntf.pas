unit TestAPIIntf;

interface

uses
  InvokeRegistry;

type
  ETestServerCommandError = class(ERemotableException)
  published
    property message; // !!! case sensitive
  end;

  TTestEnumStatus = (tesOk, tesError, tesCommandPending);

  TTestBaseResponse = class(TRemotable)
  private
    FPropEnumerable: TTestEnumStatus;
    FWideString: WideString;
  published
    property PropWideString: WideString read FWideString write FWideString;
    property PropEnumerable: TTestEnumStatus read FPropEnumerable write FPropEnumerable;
  end;

  TTestServerSimpleResponse = class(TTestBaseResponse)
  private
    FExtended: Extended;
  published
    property Extended: Extended read FExtended write FExtended;
  end;
  
  TTestServerData = class(TRemotable)
  private
    FPropInteger: Integer;
    FPropString: string;
    FPropDateTime: TDateTime;
    FBoolean: Boolean;
  published
    property PropInteger: Integer read FPropInteger write FPropInteger;
    property PropDateTime: TDateTime read FPropDateTime write FPropDateTime;
    property PropString: string read FPropString write FPropString;
    property PropBoolean: Boolean read FBoolean write FBoolean; 
  end;

  TTestServerDataResponse = class(TTestBaseResponse)
  private
    FServerData: TTestServerData;
  published
    property ServerData: TTestServerData read FServerData write FServerData;
  end;

  TTestGetArrayInputFilter = class(TRemotable)
  end;

  TTestGetArrayItem = class(TRemotable)
  private
    FNumDevice: Integer;
    FIdDevice: string;
    FNameDevice: string;
  published
    property IdDevice: string read FIdDevice write FIdDevice;
    property NameDevice: string read FNameDevice write FNameDevice;
    property NumDevice: Integer read FNumDevice write FNumDevice;
  end;

  TTestGetArrayItems = array of TTestGetArrayItem;

  TTestGetArrayResponse = class(TTestBaseResponse)
  private
    FArrayItems: TTestGetArrayItems;
  published
    property ArrayItems: TTestGetArrayItems read FArrayItems write FArrayItems;
  end;

  ITestRESTServerAPI = interface(IInvokable)
    ['{D9B9EE4B-AA8A-4BF7-BE97-8F9982A8094C}']
    function GetServerData: TTestServerDataResponse; stdcall;
    function GetSimple: TTestServerSimpleResponse; stdcall;
    function GetArray(Filter: TTestGetArrayInputFilter): TTestGetArrayResponse; stdcall;
  end;
  
implementation

uses
  SysUtils, TypInfo,
  RESTJSONConst;
  
procedure RegisterEnumMaps;
var
  LTypeInfo: PTypeInfo;
  i: TTestEnumStatus;
begin
  LTypeInfo := TypeInfo(TTestEnumStatus);
  RemClassRegistry.RegisterXSInfo(LTypeInfo);
  for i := Low(TTestEnumStatus) to High(TTestEnumStatus) do
    RemClassRegistry.RegisterExternalPropName(LTypeInfo, GetEnumName(LTypeInfo, Ord(i)), IntToStr(Ord(i)));
end;

initialization
  // register types
  InvRegistry.RegisterInterface(TypeInfo(ITestRESTServerAPI), 'ashumkin ;)', 'utf-8');
  // register enum mapping
  RegisterEnumMaps;
  // register exception class
  RemClassRegistry.RegisterXSClass(ETestServerCommandError, rsJSONErrorClass);
  // register class
  RemClassRegistry.RegisterXSClass(TTestServerDataResponse, '', 'ServerData');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(TTestServerDataResponse), 'PropEnumerable', 'Enumerable');
  RemClassRegistry.RegisterXSClass(TTestServerData);
  RemClassRegistry.RegisterExternalPropName(TypeInfo(TTestServerData), 'PropInteger', 'Integer');

  RemClassRegistry.RegisterXSClass(TTestServerSimpleResponse, '', '.');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(TTestServerSimpleResponse), 'Extended', 'FloatNumber');
  RemClassRegistry.RegisterXSClass(TTestGetArrayResponse, '', 'ListUnit');
  // register mapping
  RemClassRegistry.RegisterExternalPropName(TypeInfo(TTestGetArrayResponse), 'ArrayItems', 'ListUnit');

end.
