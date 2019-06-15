{*******************************************************}
{                                                       }
{ Borland Delphi Visual Component Library               }
{                 SOAP Support                          }
{                                                       }
{ Copyright (c) 2001 Borland Software Corporation       }
{                                                       }
{*******************************************************}

{ Converts a JSON RPC request to/from an internal Delphi format using a superobject }

unit OPToRESTJSONConv;

interface

uses SysUtils, Variants, TypInfo, Classes, IntfInfo, InvokeRegistry,
     OPConvert, JSONConv, Types, XSBuiltIns, superobject,
     Contnrs;

const
  SVarArrayType = 'VarArrayType';     { do not localize }

type

  EJSONConvertError = class(Exception);

  TJSONArrayElemDesc = record
    MultiDim: Boolean;
    Dims: TIntegerDynArray;
  end;
  TJSONArrayDesc = array of TJSONArrayElemDesc;

  ConvNodeState = (nsClientSend, nsServerReceive, nsServerSend, nsClientReceive);

  TMemberDataNotReceivedEvent = procedure(const ClassName: string; const Member: string) of object;
  TUnhandledNodeEvent  = procedure(const Name: string; NodeXML: WideString) of object;

  TJSONConv = class(TJSONProcessor, IObjConverter)
  private
    FIDs: Integer;
    FOptions: TJSONConvertOptions;
    ObjsWriting: array of TObject;
    FOnMemberDataNotReceived: TMemberDataNotReceivedEvent;
    FOnUnhandledNode: TUnhandledNodeEvent;
  protected
    function  NodeIsNULL(Node: ISuperObject): Boolean;
    function  ChildNodesAreNull(Node: ISuperObject): Boolean;
    function  CreateNULLNode(RootNode: ISuperObject; const Name: InvString): ISuperObject;
    function  GetNewID: string;
    function  GetElementType(Node: ISuperObject; var TypeName: InvString): Boolean;
    function  CreateScalarNodeXS(RootNode: ISuperObject;  const NodeName, TypeName: WideString; const Value: WideString; GenPre: Boolean = False): ISuperObject;
    function  CreateTypedNode(RootNode: ISuperObject; const NodeName: WideString; TypeName: WideString; GenPre: Boolean = False): ISuperObject;
    function  GetNodeAsText(Node: ISuperObject): InvString;
    procedure CheckEncodingStyle(Node: ISuperObject);

    procedure AddObjectAsWriting(Instance: TObject);
    procedure RemoveObjectAsWriting(Instance: TObject);
    function  IsObjectWriting(Instance: TObject): Boolean;
    procedure ResetMultiRef;
    { Methods to handle Variants }
    procedure ConvertVariantToSoap(RootNode: ISuperObject;
              const Name: InvString; Info: PTypeInfo; P: PVarData; NumIndirect: Integer; V: Variant; UseVariant: Boolean);
    procedure ConvertSoapToVariant(Node: ISuperObject; InvData: Pointer);
    function  IsNodeAVarArray(const Node: ISuperObject; var VT: TVarType): Boolean;
    procedure WriteVarArray(RootNode: ISuperObject; const Name: InvString; V: Variant);
    procedure WriteVariant(RootNode: ISuperObject; const Name: InvString; V: Variant);
    procedure ReadVariant(Node: ISuperObject; P: Pointer);
    function  ReadVarArrayDim(Node: ISuperObject; IsVarVArray: Boolean = False; VT: TVarType = 0): Variant;
    procedure WriteVarArrayAsB64(RootNode: ISuperObject; const Name: InvString; V: Variant);
    { Methods to handle native delphi array types }
    function  MakeArrayNode(RootNode: ISuperObject;  const Name, TypeName: InvString;
                            Indices: array of Integer): ISuperObject; overload;
    function  MakeArrayNode(RootNode: ISuperObject;  const Name, TypeName: InvString;
                            Dim, Len: Integer): ISuperObject; overload;
    procedure ConvertNativeArrayToSoap(RootNode: ISuperObject;
               const Name: InvString; Info: PTypeInfo; P: Pointer; NumIndirect: Integer; InlineElements: Boolean = False);
    procedure WriteNonRectDynArray(RootNode: ISuperObject; const Name: InvString; Info: PTypeInfo; const TypeName: InvString; P: Pointer; Dim: Integer);
    function  WriteNonRectDynArrayElem(RootNode: ISuperObject;  Info: PTypeInfo; const TypeName: InvString; P: Pointer; Dim: Integer): Integer;
    function  ConvertSoapToNativeArray(DataP: Pointer;  TypeInfo: PTypeInfo;
                                       RootNode: ISuperObject): Pointer;
    function  ConvertSoapToNativeArrayElem(ArrayInfo, ElemInfo: PTypeInfo;
                                           RootNode: ISuperObject; ArrayDesc: TJSONArrayDesc;
                                           Dims, CurDim: Integer; DataP: Pointer): Pointer;
    procedure ConvertByteArrayToSoap(RootNode: ISuperObject; const Name: InvString;
                                     Info: PTypeInfo; P: Pointer);
    procedure WriteRectDynArrayElem(RootNode: ISuperObject; Info: PTypeInfo; Size, Dim: Integer; P: Pointer; const TypeName: InvString);
    procedure WriteRectDynArray(RootNode: ISuperObject; Info: PTypeInfo; Dims: Integer; P: Pointer; const TypeName: InvString);
    procedure ReadRectDynArray(RootNode: ISuperObject; Info: PTypeInfo; Dims: Integer; P: Pointer; CurElem: Integer);
    procedure ReadRectDynArrayElem(RootNode: ISuperObject; Info: PTypeInfo; Size, Dim: Integer; P: Pointer; var CurElem: Integer);
    procedure ReadRow(RootNode: ISuperObject; var CurElem: Integer; Size: Integer; P: Pointer; Info: PTypeInfo);
    { Enums }
    function  ConvertEnumToSoap(Info: PTypeInfo; P: Pointer; NumIndirect: Integer): InvString;
    function  ConvertSoapToEnum(Info: PTypeInfo; S: InvString; IsNull: Boolean): Integer;

    { Methods that handle TObjects with RTTI }
    function  SerializationOptions(Cls: TClass): TSerializationOptions; overload;
    function  SerializationOptions(ATypeInfo: PTypeInfo): TSerializationOptions; overload;
    function  SerializationOptions(Obj: TObject): TSerializationOptions; overload;
    procedure ConvertObjectToSOAP(const Name: InvString; ObjP: Pointer; RootNode: ISuperObject; NumIndirect: Integer);
    function  ConvertSOAPToObject(RootNode: ISuperObject;
              AClass: TClass; const TypeName: WideString; ObjP: Pointer; NumIndirect: Integer): TObject;

    function  CreateObjectNode(Instance: TObject; RootNode: ISuperObject;
                               const Name: InvString; ObjConvOpts: TObjectConvertOptions): InvString;
    function  ObjInstanceToSOAP(Instance: TObject; RootNode: ISuperObject;
                               const NodeName: InvString; ObjConvOpts: TObjectConvertOptions;
                               out RefID: InvString): ISuperObject;
    procedure LoadObject(Instance: TObject; RootNode: ISuperObject);
    procedure InitObjectFromSOAP(Instance: TObject; RootNode: ISuperObject);
    procedure ObjectMemberNoShow(const ClassName: string; const MemberName: string);
    procedure UnhandledNode(const Name: string; NodeXML: WideString);

    procedure SetObjectPropFromText(Instance: TObject; PropInfo: PPropInfo; const SoapData: WideString);
    function  GetObjectPropAsText(Instance: TObject; PropInfo: PPropInfo): WideString;

    function  GetOptions: TJSONConvertOptions;
    procedure SetOptions(const Value: TJSONConvertOptions);
 public
    procedure ConvertNativeDataToSoap(RootNode: ISuperObject;
                 const Name: InvString; Info: PTypeInfo; P: Pointer; NumIndirect: Integer); dynamic;
    procedure ConvertSoapToNativeData(DataP: Pointer; TypeInfo: PTypeInfo;
                 Context: TDataContext; RootNode: ISuperObject; Translate, ByRef: Boolean; NumIndirect: Integer); dynamic;
  published
    property Options: TJSONConvertOptions read FOptions write FOptions;
    property OnMemberDataNotReceived: TMemberDataNotReceivedEvent read FOnMemberDataNotReceived write FOnMemberDataNotReceived;
    property OnUnhandledNode: TUnhandledNodeEvent read FOnUnhandledNode write FOnUnhandledNode;
  end;

  TOPToJSONConvert = class(TJSONConv, IOPConvert)
  private
    FTempDir:  string;
//    Envelope:  TSoapEnvelope;
    procedure  JSONToStream(const JSONDoc: ISuperObject; Stream: TStream);
    procedure  ProcessFault(FaultNode: ISuperObject);
    procedure  ProcessSuccess(RespNode: ISuperObject; const IntfMD: TIntfMetaData;
                              const MD: TIntfMethEntry; InvContext: TInvContext);
    function   GetPartName(MethMD: TIntfMetaData; const ParamName: InvString): InvString;
    function   GetTempDir: string; virtual;
    procedure  SetTempDir(const Value: string); virtual;
  protected
    function   NewSuperObject: ISuperObject;
    { Helper routine }
    function StreamToJSON(AStream: TStream): ISuperObject;
    procedure ProcessResponse(const JSONDoc: ISuperObject;
                              const IntfMD: TIntfMetaData;
                              const MD: TIntfMethEntry;
                              Context: TInvContext;
                              Headers: THeaderList); overload; virtual;
    function ResponseValid(const JSONDoc: ISuperObject;
                           const IntfMD: TIntfMetaData;
                           const MD: TIntfMethEntry;
                           Context: TInvContext): Boolean; virtual;
    function ResponseIsError(JSONDoc: ISuperObject; out ErrorNode: ISuperObject): Boolean; virtual;    
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    { IOPConvert }
    procedure MsgToInvContext(const Request: InvString; const IntfMD: TIntfMetaData;
                              var MethNum: Integer; Context: TInvContext); overload; virtual;
    procedure MsgToInvContext(const Request: TStream; const IntfMD: TIntfMetaData;
                              var MethNum: Integer; Context: TInvContext;
                              Headers: THeaderList); overload; virtual;
    function  InvContextToMsg(const IntfMD: TIntfMetaData;
                              MethNum: Integer;
                              Con: TInvContext;
                              Headers: THeaderList): TStream;
    procedure MakeResponse(const IntfMD: TIntfMetaData;
                              const MethNum: Integer;
                              Context: TInvContext;
                              Response: TStream;
                              Headers: THeaderLIst); virtual;
    procedure MakeFault(const Ex: Exception; EStream: TStream); virtual;
    procedure ProcessResponse(const Resp: TStream;
                              const IntfMD: TIntfMetaData;
                              const MD: TIntfMethEntry;
                              Context: TInvContext;
                              Headers: THeaderList); overload; virtual;
  published
    property TempDir: string read GetTempDir write SetTempDir;
  end;

function GetOrdPropEx(Instance: TObject; PropInfo: PPropInfo): Longint;

var
  DefArrayElemName: string = 'item';    { do not lcoalize }

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, ComObj,
{$ENDIF}
EncdDecd, RESTJSONConst, InvRules, TypeTrans, OPToSOAPDomCustom, VarUtils, StrUtils,
  HTTPUtil;

type

  { Add access to CacheFile : no data members! }

TConvertAttachment = class(TSOAPAttachment)
  procedure SetCacheFile(const Value: string);
end;

  procedure TConvertAttachment.SetCacheFile(const Value: string);
    begin
    SetSourceFile('');
    InternalSetCacheFile(Value);
  end;

{ util function }

function ntElementChildCount(const Node: ISuperObject): Integer;
//var
//  LEnum: TSuperAvlIterator;
begin
  Result := 0;
  if Node.IsType(stArray) then
    Result := Node.AsArray.Length;
//
//  LEnum := Node.AsObject.GetEnumerator;
//  while LEnum.MoveNext do
//    Inc(Result);
end;

function ntElementChild(const Node: ISuperObject; Index: Integer): ISuperObject;
var
  I, J: Integer;
begin
  Result := nil;
  if Node.IsType(stArray) then
    Result := Node.AsArray.O[Index];
//  if (Node = nil) or (Node.ChildNodes = nil) then
//    Exit;
//  J := 0;
//  for I := 0 to Node.ChildNodes.Count-1 do
//    if Node.ChildNodes[I].NodeType = ntElement then
//    begin
//      if (J = Index) then
//      begin
//        Result := Node.ChildNodes[I];
//        Exit;
//      end else
//        Inc(J);
//    end;
end;


procedure ParseDims(DimString: InvString; var Dims: TJSONArrayDesc);
var
  I, J: Integer;
  CurDim, NumDims, SubDims, SubDim: Integer;
  StrLen: Integer;
  DimSize: InvString;
begin
  CurDim := 0;
  NumDims := 0;
  StrLen := Length(DimString);
  for I := 1 to StrLen do
    if DimString[I] = '[' then      { do not localize }
      Inc(NumDims);
  SetLength(Dims, NumDims);
  I := 1;
  while I < StrLen do
  begin
    if DimString[I] = '[' then       { do not localize }
    begin
      DimSize := '';
      Inc(I);
      SubDims := 1;
      SubDim := 0;
      if DimString[I] = ']' then               { do not localize }
        SetLength(Dims[CurDim].Dims, 1);
      while (DimString[I] <> ']') and (I < StrLen) do     { do not localize }
      begin
        J := I;
        while (DimString[J] <> ']') and (J < StrLen) do       { do not localize }
        begin
          if DimString[J] = ',' then
            Inc(SubDims);
          Inc(J);
        end;
        SetLength(Dims[CurDim].Dims, SubDims);
        if SubDims > 1 then
        begin
          Dims[CurDim].MultiDim := True;
          while (DimString[I] <> ']') and (I < StrLen) do     { do not localize }
          begin
            DimSize := '';
            while (DimString[I] <> ',') and (DimString[I] <> ']') and (I < StrLen) do   { do not localize }
            begin
              DimSize := DimSize + DimString[I];
              Inc(I);
            end;
            if DimString[I] = ',' then
              Inc(I);
            if trim(DimSize) <> '' then
              Dims[CurDim].Dims[SubDim] := StrToInt(trim(DimSize))
            else
              Dims[CurDim].Dims[SubDim] := 0;
            Inc(SubDim);
          end
        end else
        begin
          while (DimString[I] <> ']') and (I < StrLen) do      { do not localize }
          begin
            DimSize := DimSize + DimString[I];
            Inc(I);
          end;
          if trim(DimSize) <> '' then
            Dims[CurDim].Dims[SubDim] := StrToInt(trim(DimSize))
          else
            Dims[CurDim].Dims[SubDim] := 0;
        end;
      end;
      Inc(I);
      Inc(CurDim);
    end else
      Inc(I);
  end;
end;

{ TOPToSoapDomConvert }

type
  PTObject = ^TObject;


{ Server Receives Message }
procedure TOPToJSONConvert.MsgToInvContext(const Request: InvString;
  const IntfMD: TIntfMetaData; var MethNum: Integer; Context: TInvContext);
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(Request[1], Length(Request) * 2);
    Stream.Position := 0;
    MsgToInvContext(Stream, IntfMD, MethNum, Context, nil);
  finally
    Stream.Free;
  end;
end;

procedure TOPToJSONConvert.MsgToInvContext(const Request: TStream;
  const IntfMD: TIntfMetaData; var MethNum: Integer; Context: TInvContext;
  Headers: THeaderList);
var
  JSONDoc: ISuperObject;
  I, J, K, L, ParamCount: Integer;
  MethodName, InternalMethodName, ExtParamName: InvString;
  EnvNode, MethNode, ParamNode, Node, HdrNode: ISuperObject;
  ProcessedBody: Boolean;
  MD: TIntfMethEntry;
  Translate: Boolean;
  ParamSerialOpts: TSerializationOptions;
begin
  Request.Position := 0;
  JSONDoc := TSuperObject.ParseStream(Request, True);
//  EnvNode := JSONDoc.DocumentElement;
//  if EnvNode = nil then
//    raise ESOAPDomConvertError.Create(SInvalidSOAPRequest);

  ProcessedBody := False;
//  try
//    if EnvNode.hasChildNodes then
//    begin
//      for I := 0 to EnvNode.childNodes.Count -1 do
//      begin
//        Node := EnvNode.childNodes[I];
//        if Node.NodeType <> ntElement then
//          continue;
//        if ExtractLocalName(Node.NodeName) = SSoapHeader then
//        begin
//          if Node.hasChildNodes then
//          begin
//            for L := 0 to Node.childNodes.Count-1 do
//            begin
//              HdrNode := Node.childNodes[L];
//              if HdrNode.NodeType <> ntElement then
//                continue;
//              ReadHeader(EnvNode, HdrNode, Headers);
//            end;
//          end;
//        end
//        else if ExtractLocalName(Node.NodeName) = SSoapBody then
//        begin
//          if ProcessedBody then
//            raise ESOAPDomConvertError.Create(SMultiBodyNotSupported);
//          CheckEncodingStyle(EnvNode);
//          ProcessedBody := True;
//          if Node.ChildNodes.Count > 0 then
//          begin
//            { Rather than assume that the first childNode is the method's
//              node, it would be safer to use the 'root' attribute. However,
//              SOAPBuilder can't seem to agree on 'root' currently. So for
//              now, we'll stay with this approach }
//            MethNode := ntElementChild(Node, 0);
//            CheckEncodingStyle(MethNode);
//            MethodName := ExtractLocalName(MethNode.NodeName);
//            InternalMethodName := InvRegistry.GetMethInternalName(IntfMD.Info, MethodName);
//            MethNum := GetMethNum(IntfMD, InternalMethodName, NtElementChildCount(MethNode));
//            { Here know if there's a method for the request sent }
//            if MethNum = -1 then
//                raise ESOAPDomConvertError.CreateFmt(SNoSuchMethod, [MethodName, IntfMD.Name]);
//
//            MD := IntfMD.MDA[MethNum];
//            Context.SetMethodInfo(MD);
//            Context.AllocServerData(MD);
//
//            { Get native parameters }
//            ParamCount := 0;
//            for K := 0 to Length(MD.Params)-1 do
//              if MD.Params[K].Name <> '' then
//                Inc(ParamCount);
//
//            for K := 0 to Length(MD.Params)-1 do
//            begin
//              { Skip non-parameters }
//              if MD.Params[K].Name = '' then
//                continue;
//              { Was parameter renamed ? }
//              ExtParamName := InvRegistry.GetParamExternalName(IntfMD.Info, InternalMethodName, MD.Params[K].Name);
//              ParamSerialOpts := SerializationOptions(MD.Params[K].Info);
//              for J := 0 to MethNode.childNodes.Count -1 do
//              begin
//                ParamNode := MethNode.childNodes[J];
//                if ParamNode.NodeType <> ntElement then
//                  continue;
//                { Warning: In case sensitive contexts, it's possible to have parameters
//                           that differ only in case - C++ }
//                if SameText(ExtParamName, ExtractLocalName(ParamNode.NodeName)) then
//                begin
//                  CheckEncodingStyle(ParamNode);
//                  Translate := (pfVar in MD.Params[K].Flags)
//                    or (pfConst in  MD.Params[K].Flags)
//                    or ([] =  MD.Params[K].Flags)
//                    or ((pfReference in MD.Params[K].Flags) and (MD.Params[K].Info.Kind = tkVariant))
//                    or ((pfReference in MD.Params[K].Flags) and (MD.Params[K].Info.Kind = tkString));
//                  ConvertSoapToNativeData(Context.GetParamPointer(K), MD.Params[K].Info, Context, MethNode,
//                    ParamNode, Translate, False,  1);
//                  break;
//                end
//                  { Here we have an unhandled parameter node }
//                  { Check if the name mismatches were due to wrapper classes }
//                else if (xoHolderClass in ParamSerialOpts) and (ParamCount = 1) then
//                begin
//                  Translate := (pfVar in MD.Params[K].Flags)
//                    or (pfConst in  MD.Params[K].Flags)
//                    or ([] =  MD.Params[K].Flags)
//                    or ((pfReference in MD.Params[K].Flags) and (MD.Params[K].Info.Kind = tkVariant))
//                    or ((pfReference in MD.Params[K].Flags) and (MD.Params[K].Info.Kind = tkString));
//                  ConvertSoapToNativeData(Context.GetParamPointer(K), MD.Params[K].Info, Context, MethNode,
//                                          Node, Translate, False,  1);
//                  break;
//                end else
//                begin
//                  { Could not deserialize node... }
//                  UnhandledNode(MethodName, ParamNode.XML);
//                end;
//              end;
//            end;
//          end;
//        end;
//      end;
//    end else
//      raise ESOAPDomConvertError.Create(SInvalidSOAPRequest);
//  finally
//    ResetMultiRef;
//  end;
end;

procedure TOPToJSONConvert.JSONToStream(const JSONDoc: ISuperObject; Stream: TStream);
var
  XMLWString: WideString;
  StrStr: TStringStream;
begin

  { NOTE: Typically you don't want us to UTF8 Encode if you've requested
          the DOM to encode; however, some implementations seem to
          indiscriminately UTF8Decode - so you can force UTF8 encoding, which
          will make us make the DOM ignore any encoding set
     *********************************************************************
       Remember to keep the Transport in sync. with any DOM encodings -
       namely the 'UseUTF8InHeader' property of the transport components     }

  XMLWString := JSONDoc.AsJSon;
  StrStr := TStringStream.Create(UTF8Encode(XMLWString));
  try
    Stream.CopyFrom(StrStr, 0);
  finally
    StrStr.Free;
  end;
end;

procedure TOPToJSONConvert.MakeResponse(const IntfMD: TIntfMetaData; const MethNum: Integer;
                                           Context: TInvContext; Response: TStream;
                                           Headers: THeaderList);
var
  I: Integer;
  JSONDoc: ISuperObject;
//  EnvNode, HeaderNode, BodyNode, MethNode, RootNode: ISuperObject;
  MD: TIntfMethEntry;
  SoapNS: InvString;
  ArgName: InvString;
  P: Pointer;
  Header: TObject;
begin
  MD := IntfMD.MDA[MethNum];
  JSONDoc := NewSuperObject;
//  EnvNode := Envelope.MakeEnvelope(JSONDoc, Options);
//
//  { Result the MultiRef IDs as we're about to create a new Response }
//  FIDS := 1;
//
//  if (Headers <> nil) and (Headers.Count > 0) then
//  begin
//    HeaderNode := Envelope.MakeHeader(EnvNode);
//    for I := 0 to Headers.Count-1 do
//    begin
//      Header := Headers[I];
//      WriteHeader(Header, HeaderNode, HeaderNode);
//    end;
//  end;
//
//  BodyNode := Envelope.MakeBody(EnvNode);
//
////  if not (soLiteralParams in Options) then
//  begin
////    SoapNS := GetSoapNS(IntfMD);
////    if not (soDocument in Options) then
////      MethNode := BodyNode.AddChild(MD.Name + SSoapResponseSuff, SoapNS, True)
////  end else
////  begin
//    { If Literal params were not unwound, we don't need a method node }
//    MethNode := BodyNode;
//  end;
//
//  { Compute Root Node }
//  RootNode := BodyNode;
//
//  try
//    if MD.ResultInfo <> nil  then
//    begin
//      ArgName := GetPartName(IntfMD, '');
//      ConvertNativeDataToSoap(RootNode, MethNode, ArgName, MD.ResultInfo, Context.GetResultPointer, 1);
//    end;
//    for I := 0 to MD.ParamCount - 1  do
//    begin
//      if (pfVar in MD.Params[I].Flags) or (pfOut in MD.Params[I].Flags)
//      then
//      begin
//        P := Context.GetParamPointer(I);
//        ConvertNativeDataToSoap(RootNode, MethNode, MD.Params[I].Name, MD.Params[I].Info, P, 1);
//      end;
//    end;
//  finally
//    FinalizeMultiRefNodes;
//    ResetMultiRef;
//  end;
//
  { Let DOM write to stream - DOM handles Encoding }
  JSONToStream(JSONDoc, Response);
end;

procedure TOPToJSONConvert.MakeFault(const Ex: Exception; EStream: TStream);
var
  JSONDoc: ISuperObject;
//  EnvNode, BodyNode, FaultNode, FA, FC, FS, FD, CustNode: ISuperObject;
  I, Count: Integer;
  PropList: PPropList;
  URI, TypeName: WideString;
  IsScalar: Boolean;
  RemException: ERemotableException;
begin
  JSONDoc := NewSuperObject;
//  EnvNode := Envelope.MakeEnvelope(JSONDoc, Options);
//  BodyNode := Envelope.MakeBody(EnvNode);
//  FaultNode := Envelope.MakeFault(BodyNode);
//  FA := FaultNode.AddChild(SSoapFaultActor, '');
//  FC := FaultNode.AddChild(SSoapFaultCode, '');
//  { NOTE: We map the FaultString to Exception's Message }
//  FS := FaultNode.AddChild(SSoapFaultString, '');
//  FS.Text := Ex.Message;
//  if Ex.InheritsFrom(ERemotableException) then
//  begin
//    RemException := ERemotableException(Ex);
//    FA.Text := RemException.FaultActor;
//    FC.Text := MakeNodeName('', RemException.FaultCode);
//
//    RemClassRegistry.ClassToURI(Ex.ClassType, URI, TypeName, IsScalar);
//
//    FD := FaultNode.AddChild(SSoapFaultDetails, '');
//    CustNode := FD.AddChild(TypeName, URI, True);
//
//    { Set the type }
//    CustNode.SetAttributeNS(SSoapType, XMLSchemaInstNameSpace, MakeNodeName(CustNode.Prefix, TypeName));
//
//    Count := GetTypeData(Ex.ClassInfo)^.PropCount;
//    if Count > 0 then
//    begin
//      GetMem(PropList, Count * SizeOf(Pointer));
//      try
//        GetPropInfos(Ex.ClassInfo, PropList);
//        for I := 0 to Count - 1 do
//        begin
//          if not RemTypeRegistry.TypeInfoToXSD( (PropList[I].PropType)^, URI, TypeName) then
//            raise ESOAPDomConvertError.CreateFmt(SRemTypeNotRegistered, [(PropList[I].PropType)^.Name]);
//          CreateScalarNodeXS(CustNode, CustNode, PropList[I].Name, URI, TypeName, GetObjectPropAsText(Ex, PropList[I]));
//        end;
//      finally
//        FreeMem(PropList, Count * SizeOf(Pointer));
//      end;
//    end;
//  end else
//  begin
//    { Fault Code }
//    FC.Text := MakeNodeName('', 'Server');   { Do not localize }
//  end;
  JSONToStream(JSONDoc, EStream);
end;

function TOPToJSONConvert.InvContextToMsg(const IntfMD: TIntfMetaData; MethNum: Integer;
                                             Con: TInvContext; Headers: THeaderList): TStream;
var
  JSONDoc: ISuperObject;
  I: Integer;
  SoapMethNS: InvString;
  MethMD: TIntfMethEntry;
  P: Pointer;
  Indir: Integer;
  URI, ExtMethName, ExtParamName: InvString;
  Header: TObject;
begin
  MethMD := IntfMD.MDA[MethNum];

  JSONDoc := NewSuperObject;
(*  EnvNode := Envelope.MakeEnvelope(JSONDoc, Options);

  { Result MultiRef IDs are we're about to create new request }
  FIDS := 1;

  { Any headers }
  if (Headers <> nil) and (Headers.Count > 0) then
  begin
    HeaderNode := Envelope.MakeHeader(EnvNode);
    for I := 0 to Headers.Count-1 do
    begin
      Header := Headers[I];
      WriteHeader(Header, HeaderNode, HeaderNode);
    end;
  end;

  BodyNode := Envelope.MakeBody(EnvNode);
*)

  try
    JSONDoc.S['Command'] := MethMD.Name;
    { Add each parameter to the method node }

    for I := 0 to MethMD.ParamCount - 1  do
    begin
      if not (pfOut in MethMD.Params[I].Flags) then
      begin
        { In doc|lit mode, we use the typename for the node }
//        if (soDocument in Options) and (soLiteralParams in Options) then
//          RemTypeRegistry.TypeInfoToXSD(MethMD.Params[I].Info, URI, ExtParamName)
//        else
          ExtParamName := InvRegistry.GetParamExternalName(IntfMD.Info, MethMD.Name, MethMD.Params[I].Name);
        P := Con.GetParamPointer(I);
        Indir := 1;
        if IsParamByRef(MethMd.Params[I].Flags, MethMD.Params[I].Info, MethMD.CC) then
          Inc(Indir);
        ConvertNativeDataToSoap(JSONDoc, ExtParamName, MethMD.Params[I].Info, P, Indir);
      end;
    end;
  finally
    ResetMultiRef;
  end;

  Result := TMemoryStream.Create();
  JSONToStream(JSONDoc, Result);
end;

procedure TOPToJSONConvert.ProcessSuccess(RespNode: ISuperObject;
                                             const IntfMD: TIntfMetaData;
                                             const MD: TIntfMethEntry;
                                             InvContext: TInvContext);

  { This is the tricky part of deserializing; How to determine the return index;
    This function should only be used if we're processing a function - IOW, this
    function should only be called if the method we're processing has
    MD.ResultInfo <> nil }
  function FindReturnNode: ISuperObject;
  var
    LReturnNodeName: WideString;
  begin
    Result := nil;
    RemClassRegistry.GetXSDInfoForClass(MD.ResultInfo, LReturnNodeName);
    if LReturnNodeName = '.' then
      Result := RespNode
    else if Assigned(RespNode.O[LReturnNodeName]) then
      Result := RespNode.O[LReturnNodeName];
  end;

  function IsNillable(TypeInfo: PTypeInfo): Boolean;
  begin
    Result := (TypeInfo.Kind = tkClass) or
              (TypeInfo.Kind = tkVariant);
  end;

var
  J: Integer;
  InvData: Pointer;
  ByRef: Boolean;
  Indir: Integer;
  ParamProcessed: TBooleanDynArray;
  ReturnValue: ISuperObject;
  LEnum: TSuperAvlIterator;
  LWasAChild: Boolean;
  LHaveToReturnResult: Boolean;
begin
  SetLength(ParamProcessed, MD.ParamCount);
  for J := 0 to Length(ParamProcessed) - 1 do
    ParamProcessed[J] := False;

  ReturnValue := nil;
  { Find index of return node - if we're expecting one }
  if (MD.ResultInfo <> nil) then
  begin
    ReturnValue := FindReturnNode;
    { We'll be lenient about nillable types }
    if (ReturnValue = nil) and not IsNillable(MD.ResultInfo) then
      raise EJSONConvertError.Create(SMissingJSONReturn);
  end
  else
    ReturnValue := nil;

  LWasAChild := False;
  LHaveToReturnResult := False;
  if RespNode = ReturnValue then
  begin
    LHaveToReturnResult := True;
//    InvData := InvContext.GetResultPointer;
//    ByRef := IsParamByRef([pfOut], MD.ResultInfo, MD.CC);
//    ConvertSoapToNativeData(InvData, MD.ResultInfo, InvContext, RespNode, True, ByRef, 1);
  end
  else
  begin
    { Process returned nodes }
    LEnum := RespNode.AsObject.GetEnumerator;
    while LEnum.MoveNext do
    begin
      LWasAChild := True;
      if ReturnValue = LEnum.Current.Value then
      begin
        InvData := InvContext.GetResultPointer;
        ByRef := IsParamByRef([pfOut], MD.ResultInfo, MD.CC);
        ConvertSoapToNativeData(InvData, MD.ResultInfo, InvContext, RespNode, True, ByRef, 1);
      end
      else
      begin
        J := 0;
        while J < MD.ParamCount do
        begin
          if MD.Params[J].Name = LEnum.Current.Name then
            break;
          Inc(J);
        end;
        if (J < MD.ParamCount) and not ParamProcessed[J]  then
        begin
          ParamProcessed[J] := True;
          InvData := InvContext.GetParamPointer(J);
          ByRef := IsParamByRef(MD.Params[J].Flags, MD.Params[J].Info, MD.CC);
          Indir := 1;
          if IsParamByRef(MD.Params[J].Flags, MD.Params[J].Info, MD.CC) then
            Inc(Indir);
          ConvertSoapToNativeData(InvData, MD.Params[J].Info, InvContext, LEnum.Current.Value, True, ByRef, Indir);
        end;
      end;
    end;
    LHaveToReturnResult := not LWasAChild and (MD.ResultInfo <> nil) and IsNillable(MD.ResultInfo);
  end;
  if LHaveToReturnResult then
  begin
    InvData := InvContext.GetResultPointer;
    ByRef := IsParamByRef([pfOut], MD.ResultInfo, MD.CC);
    ConvertSoapToNativeData(InvData, MD.ResultInfo, InvContext, RespNode, True, ByRef, 1);
  end;

(*  if RespNode.HasChildNodes then
  begin
    for I := 0 to RespNode.childNodes.Count - 1 do
    begin
      Node := RespNode.childNodes[I];
      { Skip non-valid nodes }
      if Node.NodeType <> ntElement then
        continue;
      { Process Return value, if we're expecting one }
      if I = RetIndex then
      begin
        InvData := InvContext.GetResultPointer;
        ByRef := IsParamByRef([pfOut], MD.ResultInfo, MD.CC);
        ConvertSoapToNativeData(InvData, MD.ResultInfo, InvContext, RespNode, Node, True, ByRef, 1);
      end
      else
      begin
        J := 0;
        while J < MD.ParamCount do
        begin
          if MD.Params[J].Name = ExtractLocalName(Node.NodeName) then
            break;
          Inc(J);
        end;
        if (J < MD.ParamCount) and not ParamProcessed[J]  then
        begin
          ParamProcessed[J] := True;
          InvData := InvContext.GetParamPointer(J);
          ByRef := IsParamByRef(MD.Params[J].Flags, MD.Params[J].Info, MD.CC);
          Indir := 1;
          if IsParamByRef(MD.Params[J].Flags, MD.Params[J].Info, MD.CC) then
            Inc(Indir);
          ConvertSoapToNativeData(InvData, MD.Params[J].Info, InvContext, RespNode, Node, True, ByRef,  Indir);
        end;
      end;
    end;
  end else if (MD.ResultInfo <> nil) and IsNillable(MD.ResultInfo) then
  begin
    InvData := InvContext.GetResultPointer;
    ByRef := IsParamByRef([pfOut], MD.ResultInfo, MD.CC);
    ConvertSoapToNativeData(InvData, MD.ResultInfo, InvContext, RespNode, nil, True, ByRef, 1);
  end;*)
end;


procedure TOPToJSONConvert.ProcessFault(FaultNode: ISuperObject);
var
  I, J: Integer;
  LMessage: WideString;
  AClass: TClass;
  Count: Integer;
  PropList: PPropList;
  Ex: ERemotableException;
begin
  Ex := nil;
  AClass := RemClassRegistry.URIToClass(rsJSONErrorClass);
  if AClass <> nil then
  begin
    if AClass.InheritsFrom(ERemotableException) then
    begin
      if FaultNode.IsType(stString) then
        LMessage := FaultNode.AsString;
      Ex := ERemotableExceptionClass(AClass).Create(LMessage);
      if not FaultNode.IsType(stString) then
        ConvertSOAPToObject(FaultNode, AClass, Ex.ClassName, Ex, 0);
//      Count := GetTypeData(Ex.ClassInfo)^.PropCount;
//      if (Count > 0) and Assigned(CustNode.ChildNodes) then
//      begin
//        GetMem(PropList, Count * SizeOf(Pointer));
//        try
//          GetPropInfos(Ex.ClassInfo, PropList);
//          for I := 0 to Count - 1 do
//          begin
//            for J := 0 to CustNode.ChildNodes.Count - 1 do
//            begin
//              if CustNode.ChildNodes[J].NodeType <> ntElement then
//                continue;
//              if ExtractLocalName(CustNode.ChildNodes[J].NodeName) = PropList[I].Name then
//                SetObjectPropFromText(Ex, PropList[I], GetNodeAsText(CustNode.ChildNodes[J]));
//            end;
//          end;
//        finally
//          FreeMem(PropList, Count * SizeOf(Pointer));
//        end;
//      end;
    end;
  end;
  { Create default SOAP invocation exception if no suitable on was found }
  if Ex = nil then
    Ex := ERemotableException.Create(LMessage);
  raise Ex;
end;


procedure TOPToJSONConvert.ProcessResponse(const JSONDoc: ISuperObject;
                                              const IntfMD: TIntfMetaData;
                                              const MD: TIntfMethEntry;
                                              Context: TInvContext;
                                              Headers: THeaderList);
var
  ErrorNode: ISuperObject;
begin
  if JSONDoc = nil then
    raise EJSONConvertError.Create(SInvalidResponse);
  try
    if not ResponseValid(JSONDoc, IntfMD, MD, Context) then
      raise EJSONConvertError.Create(SInvalidCommandResponse);
    if ResponseIsError(JSONDoc, ErrorNode) then
      ProcessFault(ErrorNode)
    else
      ProcessSuccess(JSONDoc, IntfMD, MD, Context);
  finally
    ResetMultiRef;
  end;
(*
  if EnvNode.hasChildNodes then
  begin
    for I := 0 to EnvNode.childNodes.Count -1 do
    begin
      { Skip to first ntElement node }
      Node := EnvNode.childNodes[I];
      if Node.NodeType <> ntElement then
        continue;

      { Is node a Header Node }
      if ExtractLocalName(Node.NodeName) = SSoapHeader then
      begin
        { If we've already processed header, we have an invalid Response }
        if ProcessedHeader then
          raise ESOAPDomConvertError.Create(SInvalidSOAPResponse);

        ProcessedHeader := True;
        if Node.hasChildNodes then
        begin
          for J := 0 to Node.childNodes.Count-1 do
          begin
            HdrNode := Node.childNodes[J];
            if HdrNode.NodeType <> ntElement then
              continue;
            ReadHeader(EnvNode, HdrNode, Headers);
          end;
        end;
      end
      else if ExtractLocalName(Node.NodeName) = SSoapBody then
      begin
        if RespNode <> nil then
        begin
          try
            if ExtractLocalName(RespNode.NodeName) = SSoapFault then
              ProcessFault(RespNode)
            else
              ProcessSuccess(RespNode, IntfMD, MD, Context);
          finally
            ResetMultiRef;
          end;
        end;
      end;
    end
  end else
    raise ESOAPDomConvertError.Create(SInvalidSOAPRequest);
*)
end;

procedure TOPToJSONConvert.ProcessResponse(const Resp: TStream;
                                              const IntfMD: TIntfMetaData;
                                              const MD: TIntfMethEntry;
                                              Context: TInvContext;
                                              Headers: THeaderList);
var
  JSONDoc: ISuperObject;
begin
  JSONDoc := StreamToJSON(Resp);
  ProcessResponse(JSONDoc, IntfMD, MD, Context, Headers);
end;

constructor TOPToJSONConvert.Create(AOwner: TComponent);
begin
  inherited;
//  Envelope := TSoapEnvelope.Create;
  FIDs := 1;
  Options := Options + [];
end;

destructor TOPToJSONConvert.Destroy;
begin
//  Envelope.Free;
  inherited;
end;

function TOPToJSONConvert.GetTempDir: string;
begin
  Result := FTempDir;
end;

procedure TOPToJSONConvert.SetTempDir(const Value: string);
begin
  FTempDir := Value;
  if (Value <> '') and (Value[Length(Value)] <> PathDelim) then
    FTempDir := FTempDir + PathDelim;
end;

function TOPToJSONConvert.NewSuperObject: ISuperObject;
begin
  Result := SO('{}');
end;

{ ParamName = '' implies function return value }
function TOPToJSONConvert.GetPartName(MethMD: TIntfMetaData; const ParamName: InvString): InvString;
begin
  Result := SDefaultReturnName;
end;


function TOPToJSONConvert.StreamToJSON(AStream: TStream): ISuperObject;
var
  XMLWString: WideString;
  StrStr: TStringStream;
begin
  StrStr := TStringStream.Create('');
  try
    AStream.Position := 0;
    StrStr.CopyFrom(AStream, 0);
    Result := TSuperObject.ParseString(PWideChar(UTF8Decode(StrStr.DataString)), True);
  finally
    StrStr.Free;
  end;
end;

function TOPToJSONConvert.ResponseValid(const JSONDoc: ISuperObject; const
    IntfMD: TIntfMetaData; const MD: TIntfMethEntry; Context: TInvContext):
    Boolean;
begin
  // derive and add some validation
  Result := True;
end;

function TOPToJSONConvert.ResponseIsError(JSONDoc: ISuperObject;
  out ErrorNode: ISuperObject): Boolean;
begin
  Result := False;
  ErrorNode := nil;
end;

{ TSOAPDomConv }

procedure TJSONConv.ConvertVariantToSoap(RootNode: ISuperObject;
  const Name: InvString; Info: PTypeInfo; P: PVarData; NumIndirect: Integer; V: Variant; UseVariant: Boolean);
var
  DataP: Pointer;
begin
  if UseVariant then
  begin
    if VarIsNull(V) then
      CreateNULLNode(RootNode, Name)
    else
      WriteVariant(RootNode, Name, V);
  end else
  begin
    DataP := P;
    if NumIndirect > 1 then
      DataP := Pointer(PInteger(DataP)^);
    if (DataP = nil) or VarIsNull(Variant(DataP^)) then
      CreateNULLNode(RootNode, Name)
    else
      WriteVariant(RootNode, Name, Variant(DataP^));
  end;
end;

function IsXMLDateTimeTypeInfo(const Info: PTypeInfo): Boolean;
begin
  Result := ((Info.Kind = tkClass) and (GetTypeData(Info).ClassType = TXSDateTime)) or
            ((Info.Kind = tkFloat) and (Info = TypeInfo(TDateTime)));
end;


procedure TJSONConv.WriteVariant(RootNode: ISuperObject; const Name: InvString; V: Variant);
var
  S, URI, TypeName: InvString;
  Info: PTypeInfo;
  IsScalar: Boolean;
begin
  if VarIsArray(V) then
  begin
    if VarIsType(V, varByte or varArray) then
    begin
      WriteVarArrayAsB64(RootNode, Name, V);
    end
    else
    WriteVarArray(RootNode, Name, V);
  end
  else
  begin
    if VarIsNull(V) or VarIsEmpty(V) then
       CreateNULLNode(RootNode, Name)
    else
    begin
      Info :=  RemTypeRegistry.VariantToInfo(V, False);
      if Info = nil then
         raise EJSONConvertError.Create(SUnsupportedVariant);
      RemTypeRegistry.InfoToURI(Info, TypeName, IsScalar);
      if IsXMLDateTimeTypeInfo(Info)
      {(Info.Kind = tkClass) and (GetTypeData(Info).ClassType = TXSDateTime)} then
      begin
        S := DateTimeToXMLTime(V);
      end else
        S := V;
      CreateScalarNodeXS(RootNode, Name, TypeName, S);
    end;
  end;
end;

function TJSONConv.MakeArrayNode(RootNode: ISuperObject; const Name, TypeName: InvString;
                                    Indices: array of Integer): ISuperObject;
var
  ArraySpec, Dims: InvString;
  I: Integer;
  First: Boolean;
  SoapPre, Pre: InvString;
  ID: string;
  MultiRefNode: ISuperObject;
begin
  { Assume we have a variant type and don't create an array node }
  if (TypeName = '') then
  begin
//    Result := Node.AddChild(Name);
  end else
  begin
    MultiRefNode := nil;
    Result := CreateTypedNode(RootNode, Name, SSoapEncodingArray);

    begin
      I := 0;
      if Indices[I] = 0 then
      begin
        while (I < Length(Indices) - 1) and (Indices[I] = 0) do
        begin
          Dims := Dims + '[]';    { do not localize }
          Inc(I);
        end;
      end;

      First := True;
      if I < Length(Indices)  then
      begin
        Dims := Dims + '[';          { do not localize }
        while I < Length(Indices)  do
        begin
          if not First then
          begin
            Dims := Dims + ',';      { do not localize }
          end;
          First := False;
          Dims := Dims + IntToStr(Indices[I]);
          Inc(I);
        end;
        Dims := Dims +  ']';       { do not localize }
      end;
    end;
  end;
end;

function TJSONConv.MakeArrayNode(RootNode: ISuperObject; const Name, TypeName: InvString;
                                    Dim, Len: Integer): ISuperObject;
var
  ArrayDims: TIntegerDynArray;
  I: Integer;
begin
  SetLength(ArrayDims, Dim);
  for I := 0 to Dim - 2 do
    ArrayDims[I] := 0;
  ArrayDims[Dim-1] := Len;
  Result := MakeArrayNode(RootNode,
                          Name, TypeName, ArrayDims);
end;

procedure TJSONConv.WriteVarArrayAsB64(RootNode: ISuperObject; const Name: InvString; V: Variant);
var
  I, DimCount, VSize: Integer;
  LoDim, HiDim: array of integer;
  P: Pointer;
  S, Encd: String;
  ElemNode: ISuperObject;
begin
  DimCount := VarArrayDimCount(V);
  SetLength(LoDim, DimCount);
  SetLength(HiDim, DimCount);
  for I := 1 to DimCount do
  begin
    LoDim[I - 1] := VarArrayLowBound(V, I);
    HiDim[I - 1] := VarArrayHighBound(V, I);
  end;
  VSize := 0;
  for i := 0 to DimCount - 1 do
    VSize := (HiDim[i] - LoDim[i] + 1);
  P := VarArrayLock(V);
  try
    SetString(S, PChar(P), VSize);
  finally
    VarArrayUnlock(V);
  end;
  Encd :=  EncodeString(S);
  ElemNode := CreateScalarNodeXS(RootNode, Name, 'base64Binary', Encd);   { do not localize }
end;

procedure TJSONConv.WriteVarArray(RootNode: ISuperObject; const Name: InvString; V: Variant);
var
  I, DimCount: Integer;
  LoDim, HiDim, Indices: array of integer;
  V1: Variant;
  ElemNode: ISuperObject;
  VAPropSet: Boolean;
begin
  if  not VarIsArray(V) then
  begin
    WriteVariant(RootNode, Name, V);
  end
  else
  begin
(*
    ElemNode := Node.AddChild(Name);
    DimCount := VarArrayDimCount(V);
    SetLength(LoDim, DimCount);
    SetLength(HiDim, DimCount);
    for I := 1 to DimCount do
    begin
      LoDim[I - 1] := VarArrayLowBound(V, I);
      HiDim[I - 1] := VarArrayHighBound(V, I);
    end;
    SetLength(Indices, DimCount);
    for I := 0 to DimCount - 1 do
      Indices[I] := LoDim[I];
    VAPropSet := False;
    while True do
    begin
      V1 := VarArrayGet(V, Indices);
      if VarIsArray(V1) and not VarIsType(V1, varArray or varByte) then
      begin
        WriteVarArray(RootNode, ElemNode, SDefVariantElemName, V1);
        ElemNode.SetAttributeNS(SVarArrayType, SBorlandTypeNamespace, VarType(V));
      end else
      begin
        WriteVariant(RootNode, ElemNode, SDefVariantElemName, V1);
        if not VAPropSet then
        begin
          ElemNode.SetAttributeNS(SVarArrayType, SBorlandTypeNamespace, VarType(V));
          VAPropSet := True;
        end;
      end;
      Inc(Indices[DimCount - 1]);
      if Indices[DimCount - 1] > HiDim[DimCount - 1] then
        for i := DimCount - 1 downto 0 do
          if Indices[i] > HiDim[i] then
          begin
            if i = 0 then Exit;
            Inc(Indices[i - 1]);
            Indices[i] := LoDim[i];
          end;
    end;
    *)
  end;
end;

function  TJSONConv.ReadVarArrayDim(Node: ISuperObject; IsVarVArray: Boolean; VT: TVarType): Variant;
var
  Count, I: Integer;
  SoapTypeInfo: PTypeInfo;
  ChildNode: ISuperObject;
  TypeName: InvString;
begin
  { Get count of ntElement children node }
  Count := ntElementChildCount(Node);
  if Count = 0 then
  begin
    Result := NULL;
    Exit;
  end;

  {
    Also, we could use the TVarType to (re)create a Variant of the
    original array type; Using VarVariant, however, is more
    resilient; and as long as no one cracks the Variant open - i.e.
    casts to TVarData and starts accessing members directly - we're safe.
    Sure hopes no one does that!!
  }
  Result := VarArrayCreate([0, Count -1], VarVariant);
(*
  for I := 0 to Node.ChildNodes.Count -1 do
  begin
    ChildNode := Node.ChildNodes[I];
    { Skip non-valid nodes }
    if ChildNode.NodeType <> ntElement then
      continue;
    IsVarVArray := IsNodeAVarArray(ChildNode, VT);
    if IsVarVarray or (ntElementChildCount(ChildNode) > 1) then
    begin
      Result[I] := ReadVarArrayDim(ChildNode, IsVarVArray, VT);
    end else
    begin
      if not NodeIsNULL(ChildNode) then
      begin
        GetElementType(ChildNode, TypeName);
        SoapTypeInfo := RemTypeRegistry.XSDToTypeInfo(TypeName);
        if SoapTypeInfo = nil then
          SoapTypeInfo := TypeInfo(System.WideString);
        { Handle 'dateTime' }
        if IsXMLDateTimeTypeInfo(SoapTypeInfo)
        {(SoapTypeInfo.Kind = tkClass) and (GetTypeData(SoapTypeInfo).ClassType = TXSDateTime)} then
        begin
          Result[I] := XMLTimeToDateTime(ChildNode.Text);
        end else
          Result[I] := TypeTranslator.CastSoapToVariant(SoapTypeInfo, ChildNode.Text);
      end else
        Result[I] := NULL;
    end;
  end;
*)
end;

function TJSONConv.IsNodeAVarArray(const Node: ISuperObject; var VT: TVarType): Boolean;
begin
  Result := False;
end;


procedure TJSONConv.ConvertSoapToVariant(Node: ISuperObject; InvData: Pointer);
var
  Info: PTypeInfo;
  TypeName: InvString;
  IsVarray: Boolean;
  VT: TVarType;
  Count: Integer;
begin
  { No children ?? }
//  if not Assigned(Node.ChildNodes) then
//    Exit;
  { Zero children }
  Count := ntElementChildCount(Node);
  if Count = 0 then
    Variant(PVarData(InvData)^) := NULL;
  IsVarray := IsNodeAVarArray(Node, VT);
  { TODO -oBB -cInvestigation : Why is IsTextElement relevant here? }
(*  if (Count > 1) or Node.ChildNodes[0].IsTextElement or IsVarray then
    Variant(PVarData(InvData)^) := ReadVarArrayDim(Node, IsVarray, VT)
  else
  begin
    GetElementType(Node, TypeName);
    Info := RemTypeRegistry.XSDToTypeInfo(TypeName);
    { If we can't figure out the type, map to a WideString }
    if Info = nil then
      Info := TypeInfo(System.WideString);
    { Handle dates }
    if IsXMLDateTimeTypeInfo(Info)
      {(Info.Kind = tkClass) and (GetTypeData(Info).ClassType = TXSDateTime)} then
    begin
      Variant(PVarData(InvData)^) := XMLTimeToDateTime(Node.Text);
    end else
      TypeTranslator.CastSoapToVariant(Info, GetNodeAsText(Node), InvData);
  end;*)
end;

function GetDynArrayLength(P: Pointer): Integer;
begin
  asm
    MOV  EAX, DWORD PTR P
    CALL System.@DynArrayLength
    MOV DWORD PTR [Result], EAX
  end;
end;

function RecurseArray(P: Pointer; var Dims: Integer): Boolean;
var
  I, Len, Size: Integer;
  ElemDataP: Pointer;
  Size2: Integer;
begin
  Result := True;
  if Dims > 1 then
  begin
    if not Assigned(P) then
      Exit;
    Len := GetDynArrayLength(P);
    ElemDataP := Pointer(PInteger(P)^);
    Size := GetDynArrayLength(ElemDataP);
    for I := 0 to Len - 1 do
    begin
      Size2 :=  GetDynArrayLength(ElemDataP);
      if Size <> Size2 { GetDynArrayLength(ElemDataP) } then
      begin
        Result := False;
        Exit;
      end;
      if Dims > 1 then
      begin
        Dec(Dims);
        Result := RecurseArray(ElemDataP, Dims);
        if not Result then
          Exit;
      end;
      ElemDataP := Pointer(PInteger(Pointer(Integer(P) + 4))^);
    end;
  end;
end;

function IsArrayRect(P: Pointer; Dims: Integer): Boolean;
var
  D: Integer;
begin
  D := Dims;
  Result := RecurseArray(P, D);
end;

procedure GetDims(ArrP: Pointer; DimAr: TIntegerDynArray; Dims: Integer);
var
  I: Integer;
begin
  for I := 0 to Dims - 1 do
  begin
    DimAr[I] := GetDynArrayLength(ArrP);
    if I < Dims - 1 then
    begin
      if Assigned(ArrP) then
        ArrP := Pointer(PInteger(ArrP)^);
    end;
  end;
end;

procedure TJSONConv.WriteRectDynArrayElem(RootNode: ISuperObject;
                                             Info: PTypeInfo;
                                             Size, Dim: Integer;
                                             P: Pointer;
                                             const TypeName: InvString);
var
  I: Integer;
  S: InvString;
  IsNull: Boolean;
  ArNode: ISuperObject;
  ElemSize: Integer;
  NodeName: InvString;
begin
  if Dim > 1 then
  begin
    Dec(Dim);
    for I := 0 to Size-1 do
    begin
      ElemSize := GetDynArrayLength(Pointer(PInteger(P)^));
      WriteRectDynArrayElem(RootNode, Info, ElemSize, Dim, Pointer(PInteger(P)^), TypeName);
      P := Pointer(Integer(P) + sizeof(Pointer));
    end;
  end
  else
  begin
    { Determine name of node }
//    if (soDocument in options) then
      NodeName := TypeName;
//    else
//      NodeName := DefArrayElemName;

    { Write out data }
    for I := 0 to Size-1 do
    begin
      if Info.Kind = tkClass then
      begin
        ConvertObjectToSOAP(NodeName, P, RootNode, 1);
      end else
      if Info.Kind = tkVariant then
      begin
        ConvertVariantToSoap(RootNode, NodeName, Info, P, 1, NULL, False);
      end else
      begin
        if Info.Kind = tkEnumeration then
          S := ConvertEnumToSoap(Info, P, 1)
        else
          TypeTranslator.CastNativeToSoap(Info, S, P, IsNull);

        { Create node }
//        if (soDocument in Options) then
//          ArNode := Node.AddChild(NodeName);
//        else
//          ArNode := Node.AddChild(NodeName, ''); { No namespace prefix }
        { Set Value }
//        ArNode.Text := S;
      end;
      P := Pointer( Integer(P) + GetTypeSize(Info));
    end;
  end;
end;

procedure  TJSONConv.WriteRectDynArray(RootNode: ISuperObject; Info: PTypeInfo; Dims: Integer; P: Pointer; const TypeName: InvString);
begin
  WriteRectDynArrayElem(RootNode, Info, GetDynArrayLength(P), Dims, P, TypeName);
end;

function ArrayIsNull(PObj: Pointer): Boolean;
var
  P: Pointer;
begin
  Result := not Assigned(PObj);
  if not Result then
  begin
    P := Pointer(PInteger(PObj)^);
    Result := (P = Pointer($0000));
  end;
end;

function ByteArrayInfo(ElemInfo: PTypeInfo): Boolean;
begin
  Result := ((ElemInfo.Kind = tkInteger{Pascal}) or (ElemInfo.Kind = tkChar{C++})) and
             (GetTypeData(ElemInfo).OrdType = otUByte);
end;

procedure TJSONConv.WriteNonRectDynArray(RootNode: ISuperObject;
                                            const Name: InvString;
                                            Info: PTypeInfo;
                                            const TypeName: InvString;
                                            P: Pointer; Dim: Integer);
var
  I, Len: Integer;
  ArrayNode: ISuperObject;
  ElemInfo: PTypeInfo;
  ElemName: WideString;
  PData: Pointer;
begin
  { Null Array }
  if ArrayIsNull(P) then
  begin
    CreateNullNode(RootNode, Name);
    Exit;
  end;

  { Retrieve Array Element information }
  ElemInfo := GetDynArrayNextInfo(Info);
  RemClassRegistry.TypeInfoToXSD(ElemInfo, ElemName);

  { Handle array of bytes }
  if (Dim = 1) and ByteArrayInfo(ElemInfo) then
  begin
    ConvertByteArrayToSoap(RootNode, Name, Info, P);
    Exit;
  end;

  Len := GetDynArrayLength(P);
  ArrayNode := MakeArrayNode(RootNode, Name, TypeName, Dim, Len);

  { Write elements }
  for I := 0 to Len-1 do
  begin
    { If underlying element is array, pass pointer to data }
    if ElemInfo.Kind = tkDynArray then
      PData := Pointer(PInteger(P)^)
    else
      PData := P;
    WriteNonRectDynArrayElem(ArrayNode, ElemInfo, ElemName, PData, Dim-1);
    P := Pointer(Integer(P) + GetTypeSize(ElemInfo));
  end;
end;

function TJSONConv.WriteNonRectDynArrayElem(RootNode: ISuperObject; Info: PTypeInfo;
                                               const TypeName: InvString; P: Pointer;
                                               Dim: Integer): Integer;
var
  NodeName : InvString;
begin
  Result := 0;
  { Compute NodeName }
//  if soDocument in Options then
    NodeName := TypeName;
//  else
//    NodeName := DefArrayElemName;

  { MultiD?? Recurse }
  if (Dim > 0)  or (Info.Kind = tkDynArray) then
  begin
    WriteNonRectDynArray(RootNode, NodeName, Info, TypeName, P, Dim);
  end
  else
  begin
    WriteRectDynArrayElem(RootNode, Info, 1, 1, P, TypeName);
  end;
end;

procedure TJSONConv.ReadVariant(Node: ISuperObject; P: Pointer);
var
  SoapTypeInfo: PTypeInfo;
  URI, TypeName: InvString;
begin
  Variant(PVarData(P)^) := NULL;
(*  if  Node.ChildNodes.Count > 1 then
    Variant(PVarData(P)^) := ReadVarArrayDim(Node)
  else
  begin
    GetElementType(Node, URI, TypeName);
    SoapTypeInfo := RemTypeRegistry.XSDToTypeInfo(URI, TypeName);
    if SoapTypeInfo = nil then
      SoapTypeInfo := TypeInfo(System.WideString);
   if IsXMLDateTimeTypeInfo(SoapTypeInfo)
   {(SoapTypeInfo.Kind = tkClass) and (GetTypeData(SoapTypeInfo).ClassType = TXSDateTime)} then
   begin
     Variant(PVarData(P)^) := XMLTimeToDateTime(Node.Text);
   end else
     Variant(PVarData(P)^) :=  TypeTranslator.CastSoapToVariant(SoapTypeInfo, GetNodeAsText(Node));
 end;*)
end;

procedure TJSONConv.ReadRow(RootNode: ISuperObject; var CurElem: Integer; Size: Integer; P: Pointer; Info: PTypeInfo);
var
  I: Integer;
  TypeName: InvString;
  IsNull, IsScalar: Boolean;
  ChildNode: ISuperObject;
begin
  { Make sure we're not trying to deserialize past the size
    of data we received }
  if CurElem > ntElementChildCount(RootNode) then
     raise EJSONConvertError.CreateFmt(SArrayTooManyElem, [RootNode.AsString]);
  if Info.Kind = tkClass then
  begin
    for I := 0 to Size-1 do
    begin
      RemClassRegistry.ClassToURI(GetTypeData(Info).ClassType, TypeName, IsScalar);
      ChildNode := ntElementChild(RootNode, CurElem);
      PTObject(P)^ := ConvertSOAPToObject(ChildNode,
                        GetTypeData(Info).ClassType, TypeName, P, 1);
     P := Pointer(Integer(P) + sizeof(Pointer));
     Inc(CurElem);
    end;
  end else if Info.Kind = tkVariant then
  begin
    for I := 0 to Size-1 do
    begin
//      ChildNode := ntElementChild(Node, CurElem);
      ReadVariant(ChildNode, P);
      P := Pointer(Integer(P) + GetTypeSize(Info));
      Inc(CurElem);
    end;
  end else
  begin
    IsNull := False;
    for I := 0 to Size-1 do
    begin
//      ChildNode := ntElementChild(Node, CurElem);
      ChildNode := RootNode;
      TypeTranslator.CastSoapToNative(Info,  ChildNode.AsString, P, IsNull);
      P := Pointer(Integer(P) + GetTypeSize(Info));
      Inc(CurElem);
    end;
  end;
end;


procedure TJSONConv.ReadRectDynArrayElem(RootNode: ISuperObject; Info: PTypeInfo; Size, Dim: Integer; P: Pointer;
  var CurElem: Integer);
var
  I: Integer;
  ElemSize: Integer;
begin
  if Dim > 1 then
  begin
    Dec(Dim);
    for I := 0 to Size - 1 do
    begin
      ElemSize := GetDynArrayLength(Pointer(PInteger(P)^));
      ReadRectDynArrayElem(RootNode, Info, ElemSize, Dim, Pointer(PInteger(P)^), CurElem);
      P := Pointer(Integer(P) + sizeof(Pointer));
    end;
  end
  else
  begin
     if CurElem > ntElementChildCount(RootNode) then
       raise EJSONConvertError.Create(SArrayTooManyElem);

     ReadRow(RootNode, CurElem, Size, P, Info);
  end;
end;


procedure TJSONConv.ReadRectDynArray(RootNode: ISuperObject; Info: PTypeInfo; Dims: Integer; P: Pointer; CurElem: Integer);
begin
  ReadRectDynArrayElem(RootNode, Info, GetDynArrayLength(P), Dims, P, CurElem);
end;

function TJSONConv.ConvertSoapToNativeArrayElem(ArrayInfo, ElemInfo: PTypeInfo;
  RootNode: ISuperObject; ArrayDesc: TJSONArrayDesc; Dims, CurDim: Integer; DataP: Pointer): Pointer;
var
  PElem, ChildP, DynP: Pointer;
  Size, I: Integer;
  ID: InvString;
  NodeOffset: Integer;
  CurElem: Integer;
  IntVec: TIntegerDynArray;
  DimCnt: Integer;
begin
  Result := nil;
  if Dims > 1 then
  begin
    if (Length(ArrayDesc) > 0 ) and ArrayDesc[CurDim].MultiDim then
    begin
      DynP := Pointer(PInteger(DataP)^);
      DynArraySetLength(DynP, ArrayInfo, Length(ArrayDesc[CurDim].Dims), PLongInt(ArrayDesc[CurDim].Dims));
      Result := DynP;
      Size :=  Length(ArrayDesc[CurDim].Dims);
      NodeOffset := 0;
      ReadRectDynArray(RootNode, ElemInfo, Size, DynP, NodeOffset);
    end else
    begin
      Size := ntElementChildCount(RootNode);
      DynP := Pointer(PInteger(DataP)^);
      if Length(ArrayDesc) = 0 then
      begin
        SetLength(IntVec, 1);
        DimCnt := 1;
      end else
      begin
        SetLength(IntVec, Length(ArrayDesc[CurDim].Dims));
        DimCnt := Length(ArrayDesc[CurDim].Dims);
      end;
      for I := 0 to Length(IntVec) -1 do
        IntVec[I] := ntElementChildCount(RootNode);
      DynArraySetLength(DynP, ArrayInfo, DimCnt, PLongInt(IntVec));
      PElem := DynP;
      Result := DynP;
      Dec(Dims);
      Inc(CurDim);
      for I := 0 to Size - 1 do
      begin
        ChildP :=  ConvertSoapToNativeArrayElem(GetDynArrayNextInfo(ArrayInfo), ElemInfo, RootNode,
                     ArrayDesc, Dims, CurDim, PElem);
        PInteger(PElem)^ := Integer(ChildP);
        PElem := Pointer(Integer(PElem) + sizeof(Pointer));
      end;
    end;
  end
  else if Dims = 1 then
  begin
    begin
      Size := ntElementChildCount(RootNode);
      if DataP <> nil then
      begin
        DynP := Pointer(PInteger(DataP)^);
        if Length(ArrayDesc) = 0 then
        begin
          SetLength(IntVec, 1);
          DimCnt := 1;
        end else
        begin
          SetLength(IntVec, Length(ArrayDesc[CurDim].Dims));
          DimCnt := Length(ArrayDesc[CurDim].Dims);
        end;
        for I := 0 to Length(IntVec) -1 do
          IntVec[I] := ntElementChildCount(RootNode);
        DynArraySetLength(DynP, ArrayInfo, DimCnt,  PLongInt(IntVec) );
        PElem := DynP;
        Result := DynP;
        CurElem := 0;
        if Size > 0 then
          ReadRow(RootNode, CurElem,  Size, PElem, ElemInfo);
      end;
    end;
  end;
end;

function TJSONConv.ConvertSoapToNativeArray(DataP: Pointer;  TypeInfo: PTypeInfo;
                                               RootNode: ISuperObject): Pointer;
var
  Dims: Integer;
  ElemInfo: PTypeInfo;
  ArrayDesc: TJSONArrayDesc;
  ArrayType: InvString;
  V : Variant;
  TypeName: InvString;
  S: String;
  ArrayLen: Integer;
  DynP: Pointer;
begin
//  GetElementType(Node, TypeName);
  GetDynArrayElTypeInfo(TypeInfo, ElemInfo, Dims);
  if ElemInfo = nil then
    raise  EJSONConvertError.CreateFmt(SNoArrayElemRTTI, [TypeInfo.Name]);
  if (Dims = 1) and
     ((ElemInfo.Kind = tkInteger) or (ElemInfo.Kind = tkChar)) and
     (GetTypeData(ElemInfo).OrdType = otUByte) and
     { Some SOAP implementations don't send the XML Namespace!! }
     (({(TypeURI = SXMLSchemaURI_2001) and }(TypeName = 'base64Binary')) or
      (TypeName = 'base64') or
      { Some SOAP implementations don't send the type!! }
      (TypeName = '' )) then
  begin
    S := DecodeString(RootNode.AsString);
    ArrayLen := Length(S);
    DynP := Pointer(PInteger(DataP)^);
    DynArraySetLength(DynP, TypeInfo, 1, @ArrayLen);
    Move(S[1], DynP^, Length(S));
    Result := DynP;
  end else
  begin
(*    V := Node.GetAttributeNS(SSoapEncodingArrayType, SSoap11EncodingS5);
    if not VarIsNull(V) then
    begin
      ArrayType := V;
      ArrayType := Copy(ArrayType, Pos('[',ArrayType), High(Integer));     { do not localize }
    end;*)

    (* -- Allow array of variants to be MultiD --
    if ElemnInfo.Kind = tkVariant then
    begin
      SetLength(ArrayDesc, Dims);
      SetLength(ArrayDesc[0].Dims, 1);
    end;
    *)
    ParseDims(ArrayType, ArrayDesc);
    Result := ConvertSoapToNativeArrayElem(TypeInfo, ElemInfo, RootNode, ArrayDesc, Dims, 0, DataP);
  end;
end;

function TJSONConv.GetNewID: string;
begin
  Result := IntToStr(FIDs);
  Inc(FIDs);
end;

function GetOrdPropEx(Instance: TObject; PropInfo: PPropInfo): Longint;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        { <-    EAX Longint result              }

        PUSH    EBX
        PUSH    EDI
        MOV     EDI,[EDX].TPropInfo.PropType
        MOV     EDI,[EDI]
        MOV     BL,otSLong
        CMP     [EDI].TTypeInfo.Kind,tkClass
        JE      @@isClass
        CMP     [EDI].TTypeInfo.Kind,tkDynArray
        JE      @@isDynArray
        CMP     [EDI].TTypeInfo.Kind,tkInterface
        JE      @@isInterface
        XOR     ECX,ECX
        MOV     CL,[EDI].TTypeInfo.Name.Byte[0]
        MOV     BL,[EDI].TTypeInfo.Name[ECX+1].TTypeData.OrdType
@@isDynArray:
@@isInterface:
@@isClass:
        MOV     ECX,[EDX].TPropInfo.GetProc
        CMP     [EDX].TPropInfo.GetProc.Byte[3],$FE
        MOV     EDX,[EDX].TPropInfo.Index
        JB      @@isStaticMethod
        JA      @@isField

        {       the GetProc is a virtual method }
        MOVSX   ECX,CX                  { sign extend slot offs }
        ADD     ECX,[EAX]               { vmt   + slotoffs      }
        CALL    dword ptr [ECX]         { call vmt[slot]        }
        JMP     @@final

@@isStaticMethod:
        CALL    ECX
        JMP     @@final

@@isField:
        AND     ECX,$00FFFFFF
        ADD     ECX,EAX
        MOV     AL,[ECX]
        CMP     BL,otSWord
        JB      @@final
        MOV     AX,[ECX]
        CMP     BL,otSLong
        JB      @@final
        MOV     EAX,[ECX]
@@final:
        CMP     BL,otSLong
        JAE     @@exit
        CMP     BL,otSWord
        JAE     @@word
        CMP     BL,otSByte
        MOVSX   EAX,AL
        JE      @@exit
        AND     EAX,$FF
        JMP     @@exit
@@word:
        MOVSX   EAX,AX
        JE      @@exit
        AND     EAX,$FFFF
@@exit:
        POP     EDI
        POP     EBX
end;


procedure SetOrdPropEx(Instance: TObject; PropInfo: PPropInfo;
  Value: Longint); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Value                       }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EDX

        MOV     ESI,[EDI].TPropInfo.PropType
        MOV     ESI,[ESI]
        MOV     BL,otSLong
        CMP     [ESI].TTypeInfo.Kind,tkClass
        JE      @@isClass
        CMP     [ESI].TTypeInfo.Kind,tkDynArray
        JE      @@isDynArray
        CMP     [ESI].TTypeInfo.Kind,tkInterface
        JE      @@isInterface
        XOR     EBX,EBX
        MOV     BL,[ESI].TTypeInfo.Name.Byte[0]
        MOV     BL,[ESI].TTypeInfo.Name[EBX+1].TTypeData.OrdType
@@isDynArray:
@@isInterface:
@@isClass:
        MOV     EDX,[EDI].TPropInfo.Index       { pass Index in DX      }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX     }
@@hasIndex:
        MOV     ESI,[EDI].TPropInfo.SetProc
        CMP     [EDI].TPropInfo.SetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

        {       SetProc turned out to be a virtual method. call it      }
        MOVSX   ESI,SI                          { sign extend slot offset }
        ADD     ESI,[EAX]                       { vmt   + slot offset   }
        CALL    dword ptr [ESI]
        JMP     @@exit

@@isStaticMethod:
        CALL    ESI
        JMP     @@exit

@@isField:
        AND     ESI,$00FFFFFF
        ADD     EAX,ESI
        MOV     [EAX],CL
        CMP     BL,otSWord
        JB      @@exit
        MOV     [EAX],CX
        CMP     BL,otSLong
        JB      @@exit
        MOV     [EAX],ECX
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;


function TJSONConv.SerializationOptions(ATypeInfo: PTypeInfo): TSerializationOptions;
begin
  if ATypeInfo.Kind = tkClass then
    Result := SerializationOptions(GetTypeData(ATypeInfo).ClassType)
  else
    Result := [];
end;

function TJSONConv.SerializationOptions(Obj: TObject): TSerializationOptions;
begin
  Result := SerializationOptions(Obj.ClassType);
  if Obj.InheritsFrom(TRemotable) then
    Result := Result + TRemotable(Obj).SerializationOptions;
end;

function TJSONConv.SerializationOptions(Cls: TClass): TSerializationOptions;
begin
  Result := RemTypeRegistry.SerializeOptions(Cls);
end;

function TJSONConv.CreateObjectNode(Instance: TObject; RootNode: ISuperObject;
                                       const Name: InvString;
                                       ObjConvOpts: TObjectConvertOptions): InvString;
begin
  { Allow TRemotable_xxxx classes to perform custom serialization }
  if Assigned(Instance) and Instance.InheritsFrom(TRemotable) then
    TRemotable(Instance).ObjectToSOAP(RootNode, Self, Name, ObjConvOpts, Result)
  else
    ObjInstanceToSOAP(Instance, RootNode, Name, ObjConvOpts, Result);
end;

function TJSONConv.ObjInstanceToSOAP(Instance: TObject; RootNode: ISuperObject;
                                        const NodeName: InvString; ObjConvOpts: TObjectConvertOptions;
                                        out RefID: InvString): ISuperObject;
var
  ID, Pre: InvString;
  I, Count: Integer;
  PropList: PPropList;
  Kind: TTypeKind;
  V: Variant;
  Obj: TObject;
  TypeName, NodeVal: InvString;
  PrefixNode, InstNode, ElemNode, AttrNode: ISuperObject;
  P: Pointer;
  ExtPropName: InvString;
  UsePrefix, SerializeProps, CanHaveType: Boolean;
  SerialOpts: TSerializationOptions;
  ClsType: TClass;
begin
  { Get a new ID for this node - in case we're MultiRefing... }
  RefID := GetNewID;

  { Retrieve the Serializatin options of this class }
  SerialOpts := SerializationOptions(Instance);

  { Object Custom Serialization flags }
  UsePrefix      := not (ocoDontPrefixNode in ObjConvOpts);
  SerializeProps := not (ocoDontSerializeProps in ObjConvOpts);
  CanHaveType    := not (ocoDontPutTypeAttr in ObjConvOpts);

  { Get namespace prefix }
  PrefixNode := RootNode;

  Pre := '';

  { Create the Node, if necessary }
  RemClassRegistry.ClassToURI(Instance.ClassType, TypeName);

  { Set Result Node }
  Result := InstNode;

  { Can this type generate xsi:type attributes?? }
  if CanHaveType then
  begin
    { Retrieve Type Namespace }
    RemClassRegistry.ClassToURI(Instance.ClassType, TypeName);
  end;

  { Serialize Published Properties ?? }
  if SerializeProps then
  begin
    { Serialized published properties }
    Count := GetTypeData(Instance.ClassInfo)^.PropCount;
    if Count > 0 then
    begin
      GetMem(PropList, Count * SizeOf(Pointer));
      try
        GetPropInfos(Instance.ClassInfo, PropList);

        { Complex type as wrapper of a simple type }
//        if (xoSimpleTypeWrapper in SerialOpts) and (Count = 1) then
//        begin
          NodeVal := GetObjectPropAsText(Instance, PropList[0]);
//          InstNode.Text := NodeVal;
//        end else
//        begin
//          for I := 0 to Count - 1 do
//          begin
//            ExtPropName := RemTypeRegistry.GetExternalPropName(Instance.ClassInfo, PropList[I].Name);
//            Kind := (PropList[I].PropType)^.Kind;
//            { Class Property }
//            if Kind = tkClass then
//            begin
//              Obj := GetObjectProp(Instance, PropList[I]);
//              if Obj = nil then
//              begin
////                if not (soDontSendEmptyNodes in Options) then
////                  CreateNULLNode(RootNode, InstNode, ExtPropName)
//              end
//              else
//              begin
//                ClsType := GetTypeData((PropList[I].PropType)^).ClassType;
//                RemClassRegistry.ClassToURI(ClsType, TypeName);
//
//                if IsObjectWriting(Obj) then
//                  raise ESOAPDomConvertError.CreateFmt(SNoSerializeGraphs, [Obj.ClassName]);
//                AddObjectAsWriting(Instance);
//                { NOTE: prefix for nested types ?? }
//                CreateObjectNode(Obj, RootNode, InstNode, ExtPropName, ObjConvOpts);
//                RemoveObjectAsWriting(Obj);
//              end;
//            { Array property }
//            end else if Kind = tkDynArray then
//            begin
//              P := Pointer(GetOrdPropEx(Instance, PropList[I]));
//              ConvertNativeArrayToSoap(RootNode, InstNode, ExtPropName,
//                                       (PropList[I].PropType)^, P, 0,
//                                       (xoInlineArrays in SerialOpts));
//            { Variant property }
//            end else if Kind = tkVariant then
//            begin
//               V := GetVariantProp(Instance, PropList[I]);
//               ConvertVariantToSoap(RootNode, InstNode, ExtPropName, nil, nil, 0, V, True);
//            end else
//            { Simple type property ?? }
//            begin
//              if not RemTypeRegistry.TypeInfoToXSD((PropList[I].PropType)^, TypeName) then
//                raise ESOAPDomConvertError.CreateFmt(SRemTypeNotRegistered, [(PropList[I].PropType)^.Name]);
//              { Here we check the stored property flag - that's the flag to use an
//                attribute instead of a separate node - if the property is marked
//                stored False, we'll use an attribute instead }
//              if not IsStoredProp(Instance, PropList[I]) then
//              begin
//                { Typically attributes go on the root/instance node. However, in some
//                  cases the class serializes members and then the attribute goes on
//                  the last member; this option allows attributes on specific members }
//                AttrNode := InstNode;
//                if (xoAttributeOnLastMember in SerialOpts) then
//                begin
//                  if ntElementChildCount(InstNode) > 0 then
//                    AttrNode := ntElementChild(InstNode, ntElementChildCount(InstNode)-1);
//                end;
//                NodeVal := GetObjectPropAsText(Instance, PropList[I]);
//                { Check if user does not want to send empty nodes }
////                if (not (soDontSendEmptyNodes in Options)) or (NodeVal <> '') then
////                  AttrNode.Attributes[ExtPropName] := NodeVal;
//              end
//              else
//              begin
//                NodeVal := GetObjectPropAsText(Instance, PropList[I]);
//                { Check if user does not want to send empty nodes }
////                if (not (soDontSendEmptyNodes in Options)) or (NodeVal <> '') then
//                  ElemNode := CreateScalarNodeXS(RootNode, InstNode, ExtPropName, TypeName, NodeVal);
//              end;
//            end;
//          end;
//        end;
      finally
        FreeMem(PropList, Count * SizeOf(Pointer));
      end;
    end;
  end;
end;


procedure TJSONConv.ConvertObjectToSOAP(const Name: InvString;
  ObjP: Pointer; RootNode: ISuperObject; NumIndirect: Integer);
var
  ElemNode: ISuperObject;
  I: Integer;
  ID: string;
  TypeName: WideString;
  P: Pointer;
  Instance: TObject;
begin
  P := ObjP;
  for I := 0 to NumIndirect - 1 do
    P := Pointer(PInteger(P)^);
  Instance := P;

  if Assigned(Instance) and not Instance.InheritsFrom(TRemotable) then
    raise EJSONConvertError.CreateFmt(SUnsuportedClassType, [Instance.ClassName]);

  if not Assigned(Instance) then
    CreateNULLNode(RootNode, Name)
  else
  begin
    { Retrieve URI of Type }
    if not RemClassRegistry.ClassToURI(Instance.ClassType, TypeName) then
      raise EJSONConvertError.CreateFmt(SRemTypeNotRegistered, [Instance.ClassName]);
    { NOTE: SOAP Attachments will enter this path as they are never multirefed }
    if IsObjectWriting(Instance) then
      raise EJSONConvertError.CreateFmt(SNoSerializeGraphs, [Instance.ClassName]);
    AddObjectAsWriting(Instance);
    { NOTE: Prefixing nodes can cause problems with some
            SOAP implementations. However, not doing so causes problems
            too ?? }
    CreateObjectNode(Instance, RootNode, Name, [ocoDontPrefixNode]);
    RemoveObjectAsWriting(Instance);
  end;
end;


function TJSONConv.GetObjectPropAsText(Instance: TObject;
  PropInfo: PPropInfo): WideString;
var
 I: LongInt;
 E: Extended;
 I64: Int64;
 DT: TDateTime;
begin
  case (PropInfo.PropType)^.Kind of
    tkInteger:
      begin
        I := GetOrdProp(Instance, PropInfo);
        Result := IntToStr(I);
      end;
    tkFloat:
      begin
        E := GetFloatProp(Instance, PropInfo);
        if PropInfo.PropType^ = TypeInfo(TDateTime) then
        begin
          DT := E;
          Result := DateTimeToXMLTime(DT);
        end
        else
          Result := FloatToStrEx(E);
      end;
    tkWString:
      Result := GetWideStrProp(Instance, PropInfo);
    tkString,
    tkLString:
      Result := GetStrProp(Instance, PropInfo);
    tkInt64:
      begin
        I64 := GetInt64Prop(Instance, PropInfo);
        Result := IntToStr(I64);
      end;
    tkEnumeration:
      begin
        Result := GetEnumProp(Instance, PropInfo);
        if PropInfo.PropType^ = TypeInfo(System.Boolean) then
          Result := Lowercase(Result);
      end;
    tkChar:
      begin
        I := GetOrdProp(Instance, PropInfo);
        Result :=  InvString(Char(I));
      end;
    tkWChar:
      begin
        I := GetOrdProp(Instance, PropInfo);
        Result :=  InvString(WideChar(I));
      end;
    tkClass:
      ;
    tkSet,
    tkMethod,

    tkArray,
    tkRecord,
    tkInterface,


    tkDynArray,
    tkVariant:
      raise EJSONConvertError.CreateFmt(SUnexpectedDataType, [KindNameArray[(PropInfo.PropType)^.Kind]]);

  end;

end;

function TJSONConv.GetElementType(Node: ISuperObject; var TypeName: InvString): Boolean;
var
  S : InvString;
  V: Variant;
  Pre: InvString;
begin
  TypeName := '';
  Result := False;
//  V := GetTypeBySchemaNS(Node, XMLSchemaInstNameSpace);
//  if VarIsNull(V) then
//    V := Node.GetAttribute(SSoapType);
//  if VarIsNull(V) then
//    V := Node.GetAttribute(SSoapType);
  if not VarIsNull(V) then
  begin
    S := V;
    TypeName := S;
    Result := True;
  end;
end;

procedure TJSONConv.SetObjectPropFromText(Instance: TObject; PropInfo: PPropInfo; const SoapData: WideString);
var
 I: LongInt;
 E: Extended;
 I64: Int64;
begin
  case (PropInfo.PropType)^.Kind of
    tkInteger:
      begin
        I := StrToInt(SoapData);
        SetOrdProp(Instance, PropInfo, I);
      end;
    tkFloat:
      begin
        if PropInfo.PropType^ = TypeInfo(TDateTime) then
        begin
          E := XMLTimeToDateTime(SoapData);
        end
        else
          E := StrToFloatEx(SoapData);
        SetFloatProp(Instance, PropInfo, E);
      end;
    tkWString:
      SetWideStrProp(Instance, PropInfo, SoapData);
    tkString,
    tkLString:
       SetStrProp(Instance, PropInfo, SoapData);

    tkInt64:
      begin
        I64 := StrToInt64(SoapData);
        SetInt64Prop(Instance, PropInfo, I64);
      end;
    tkEnumeration:
      SetEnumPropEx(Instance, PropInfo, SoapData);
    tkChar,
    tkWChar:
      if SoapData <> '' then
        SetOrdProp(Instance, PropInfo, Integer(SoapData[1]));
    tkClass:
      ;
    tkSet,
    tkMethod,
    tkArray,
    tkRecord,
    tkInterface,
    tkDynArray,
    tkVariant:
      raise EJSONConvertError.CreateFmt(SUnexpectedDataType, [KindNameArray[(PropInfo.PropType)^.Kind]]);

  end;
end;

{ This event is a convenient way to find out if a particular
  member of a class was not deserialized off the wire }
procedure TJSONConv.ObjectMemberNoShow(const ClassName: string; const MemberName: string);
begin
  if Assigned(FOnMemberDataNotReceived) then
    FOnMemberDataNotReceived(ClassName, MemberName);
end;

procedure TJSONConv.UnhandledNode(const Name: string; NodeXML: WideString);
begin
  if Assigned(FOnUnhandledNode) then
    FOnUnhandledNode(Name, NodeXML);
end;

procedure TJSONConv.LoadObject(Instance: TObject; RootNode: ISuperObject);
begin
  if Instance.InheritsFrom(TRemotable) then
    TRemotable(Instance).SOAPToObject(RootNode, Self)
  else
    InitObjectFromSOAP(Instance, RootNode);
end;

procedure TJSONConv.InitObjectFromSOAP(Instance: TObject; RootNode: ISuperObject);

var
  ProcessedNodes: TBooleanDynArray;
  ChildNode: ISuperObject;
  PropList: PPropList;
  Count, NodeCount: Integer;
  Kind: TTypeKind;
  I, K: Integer;
  Obj: TObject;
  IsNull: Boolean;
  TypeName: InvString;
  ArrayPtr: Pointer;
  V: Variant;
  SoapTypeInfo: PTypeInfo;
  ExternalPropName: WideString;
  SerialOpts: TSerializationOptions;
  ID: InvString;
  AttrNode: ISuperObject;
begin
  SerialOpts := SerializationOptions(Instance);

  { If we have a holder class, it's to pick up properties [unless we're
    in literal mode or inlining arrays ] }
//  if (xoHolderClass in SerialOpts) then
//  begin
//    if not (xoInlineArrays in SerialOpts) or (xoLiteralParam in SerialOpts) then
//    begin
//      { Store the data node that was destined for this class }
//      { And move up to pick up other attribute/members... }
//      HolderNode := Node;
////      Node := Node.ParentNode;
//    end;
//    { SimpleHolder - implies we're interested in only one node }
//    SimpleHolder := (xoAttributeOnLastMember in SerialOpts);
//  end;

  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      { Iterate through properties matching them to nodes or attributes }
      GetPropInfos(Instance.ClassInfo, PropList);

      { Complex type as simple type wrapper }
//      if (xoSimpleTypeWrapper in SerialOpts) and (Count = 1) then
//      begin
//        SetObjectPropFromText(Instance, PropList[0], GetNodeAsText(Node));
//      end
//      else
      begin
        { If we're not handling a holder, keep track of nodes we process }
        { A simple holder is only interested in it's data node           }
//        if not SimpleHolder then
//        begin
//          NodeCount := Node.ChildNodes.Count;
//          SetLength(ProcessedNodes, NodeCount);
//          for I := 0 to NodeCount-1 do
//            ProcessedNodes[I] := False;
//        end;
//        end else
          SetLength(ProcessedNodes, 0);

        for I := 0 to Count-1 do
        begin
          Kind := (PropList[I].PropType)^.Kind;
//          IsAttribute := not IsStoredPropConst(nil, PropList[I])
//                         and (Kind <> tkClass)
//                         and (Kind <> tkDynArray)
//                         and (Kind <> tkVariant);
          ExternalPropName := RemTypeRegistry.GetExternalPropName(Instance.ClassInfo, PropList[I].Name);

          { Is the property coming down as an attribute }
//          if IsAttribute then
//          begin
            { Get the potential attribute Node }
//            if SimpleHolder then
//              AttrNode := HolderNode
//            else
//              AttrNode := Node;

//            if AttrNode.HasAttribute(ExternalPropName) then
//              SetObjectPropFromText(Instance, PropList[I], AttrNode.Attributes[ExternalPropName])
//            else
              { Here something we were expecting did *NOT* come down the wire ?? }
//              ObjectMemberNoShow(Instance.ClassName, PropList[I].Name);
//            continue;
//          end
//          else
          begin
//            if not SimpleHolder then
//              K := FindPropNode(Node, ExternalPropName);
//            else
//              K := Node.ChildNodes.IndexOf(HolderNode);
            K := 0;
            ChildNode := RootNode.O[ExternalPropName];
            { If we have a node to deserialize }
            if Assigned(ChildNode) then
            begin
              { Mark node as processed }
              if K < Length(ProcessedNodes) then
                ProcessedNodes[K] := True;

              { Get Child with data we want node }
              { Here we match the property to a Child Node }
              if Kind = tkClass then
              begin
                Obj := ConvertSOAPToObject(ChildNode, GetTypeData((PropList[I].PropType)^).ClassType,
                  '', nil, 0);
                if Obj <> nil then
                  SetObjectProp(Instance, PropList[I], Obj);
              end
              else if Kind = tkDynArray then
              begin
                IsNull := NodeIsNull(ChildNode);
                { In document mode, the node could have attributes that we want to retrieve }
                if (not IsNull) (*or (soDocument in Options)*) then
                begin
//                  GetElementType(ChildNode, TypeName);
//                  ArrayPtr := nil;
                  { Here if the object we're writing to inlines members, then here we pass a parent node }
//                  if (xoinlineArrays in SerialOpts) then
//                    ChildNode := ChildNode.ParentNode;
                  ArrayPtr := ConvertSoapToNativeArray(@ArrayPtr, (PropList[I].PropType)^,
                                                       ChildNode);
                  SetOrdPropEx(Instance, PropList[I], Integer(ArrayPtr));
                end;
              end
              else if Kind = tkVariant then
              begin
//                if ChildNode.ChildNodes.Count > 1 then
//                  V := ReadVarArrayDim(ChildNode)
//                else
                begin
                  if NodeIsNull(ChildNode) then
                    V := NULL
                  else
                  begin
                    GetElementType(ChildNode, TypeName);
                    SoapTypeInfo := TypeInfo(System.WideString);
                    if IsXMLDateTimeTypeInfo(SoapTypeInfo)
                      {(SoapTypeInfo.Kind = tkClass) and (GetTypeData(SoapTypeInfo).ClassType = TXSDateTime)} then
                    begin
                      V := XMLTimeToDateTime(ChildNode.AsString);
                    end else
                      V := TypeTranslator.CastSoapToVariant(SoapTypeInfo, ChildNode.AsString);
                  end;
                end;
                SetVariantProp(Instance, PropList[I], V);
              end
              else
                SetObjectPropFromText(Instance, PropList[I], GetNodeAsText(ChildNode));
            end
            else
              ObjectMemberNoShow(Instance.ClassName, PropList[I].Name);
          end;
        end;

        { Here we report on Nodes that we did not deserialize }
        for I := 0 to Length(ProcessedNodes) - 1 do
        begin
          if not ProcessedNodes[I] then
//            UnhandledNode(Instance.ClassName, I);
        end;
      end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;
end;


function  TJSONConv.ConvertSOAPToObject(RootNode: ISuperObject; AClass: TClass;
  const TypeName: WideString; ObjP: Pointer; NumIndirect: Integer): TObject;
var
  IsScalar: Boolean;
  Obj: TObject;
  P: Pointer;
  I: Integer;
  NodeClass: TClass;
  NodeTypeName: InvString;
  LegalRef: Boolean;
  S: string;
begin
  P := ObjP;
  for I := 0 to NumIndirect - 1 do
    P := Pointer(PInteger(P)^);
  Obj := TObject(P);

//  GetElementType(ObjNode, NodeTypeName);
//  NodeClass := RemTypeRegistry.URIToClass(NodeTypeName, IsScalar);
  NodeClass := nil;
  LegalRef := True;
  if Assigned(Obj) then
  begin
    try
      if Obj.ClassType <> nil then
        LegalRef := True;
    except
      LegalRef := False;
    end;
  end;
  if Assigned(Obj) and  LegalRef then
  begin
    if (NodeClass <> nil) and (NodeClass <> Obj.ClassType) then
      Obj := NodeClass.Create;
  end else
  begin
  if (NodeClass <> nil) and NodeClass.InheritsFrom(AClass) then
    Obj := TRemotableClass(NodeClass).Create
  else
    Obj := TRemotableClass(AClass).Create;
  end;
  Result := Obj;

  LoadObject(Obj, RootNode);
end;

procedure TJSONConv.ConvertByteArrayToSoap(RootNode: ISuperObject; const Name: InvString;
                                              Info: PTypeInfo; P: Pointer);
var
  S, S1: String;
begin
  SetLength(S, GetDynArrayLength(P));
  Move(P^, S[1], Length(S));
  S1 :=  EncodeString(S);
  CreateScalarNodeXS(RootNode, Name, 'base64Binary', S1); { do not localize }
end;

procedure TJSONConv.ConvertNativeArrayToSoap(RootNode: ISuperObject;
    const Name: InvString; Info: PTypeInfo; P: Pointer; NumIndirect: Integer; InlineElements: Boolean);
var
  Dims, I: Integer;
  DimAr: TIntegerDynArray;
  TypeName: InvString;
  ElemNode: ISuperObject;
  ElemInfo: PTypeInfo;
  UseNonRect: Boolean;
begin
  for I := 0 to NumIndirect - 1 do
    P := Pointer(PInteger(P)^);

  { Retrieve dimensions and most-underlying element }
  Dims := 0;
  GetDynArrayElTypeInfo(Info, ElemInfo, Dims);
  { Make sure we have RTTI for element }
  if not RemTypeRegistry.TypeInfoToXSD(ElemInfo, TypeName) then
    raise EJSONConvertError.CreateFmt(SRemTypeNotRegistered, [ElemInfo.Name]);

  { Rectangular vs. Non-rectangular writers?? }
  UseNonRect := Assigned(P) and ((IsArrayRect(P, Dims)=False) or
                                 ((Dims > 1) or
                                 ByteArrayInfo(ElemInfo)
                                 ));
  if not UseNonRect then
  begin
    SetLength(DimAr, Dims);
    if Assigned(P) then
      GetDims(P, DimAr, Dims);
    GetDynArrayElTypeInfo(Info, ElemInfo, Dims);
    { Array of bytes is handled separately - serialized as base64 }
    if (Dims = 1) and ByteArrayInfo(ElemInfo) then
    begin
      ConvertByteArrayToSoap(RootNode, Name, Info, P);
    end else
    begin
      if not InlineElements then
      begin
        ElemNode := MakeArrayNode(RootNode,  Name, TypeName, DimAr);
      end
      else
      begin
        { Here we're inlining the array members }
        ElemNode := RootNode;
        { The array elements get the typename }
        TypeName := Name;
      end;
      WriteRectDynArray(ElemNode, ElemInfo, Dims, P, TypeName);
      { Not exactly optimal approach - but works for now -
        Check if user does not want to send empty nodes and snip
        this node if it has no child nodes - another approach would
        be not to parent the array node and wait until we know but... ?? }
//      if (soDontSendEmptyNodes in Options) and
//         (ElemNode.ChildNodes.Count < 1) then
//      begin
//        Node.ChildNodes.Delete(Node.ChildNodes.IndexOf(ElemNode));
//      end;
    end;
  end else
  begin
    WriteNonRectDynArray(RootNode, Name, Info, TypeName, P, Dims);
    { NOTE: For now I'm not putting the snip empty node code in non rectangular
            arrays as there has not been a need for this here yet }
  end;
end;


procedure TJSONConv.ConvertNativeDataToSoap(RootNode: ISuperObject;
                const Name: InvString; Info: PTypeInfo; P: Pointer; NumIndirect: Integer);
var
  ElemNode: ISuperObject;
  TypeName: InvString;
  S: InvString;
  IsNull: Boolean;
  I: Integer;
  IsScalar: Boolean;
begin
  case Info.Kind of
    tkClass:
      ConvertObjectToSOAP(Name, P, RootNode, NumIndirect);
    tkDynArray:
      ConvertNativeArrayToSoap(RootNode, Name, Info, P, NumIndirect);
    tkSet,
    tkMethod,
    tkArray,
    tkRecord,
    tkInterface:
      raise EJSONConvertError.CreateFmt(SDataTypeNotSupported, [KindNameArray[Info.Kind]]);

    tkVariant:
    begin
      ConvertVariantToSoap(RootNode, Name, Info, P, NumIndirect, NULL, False);
    end;
    else
    begin
      if Info.Kind = tkEnumeration then
      begin
        if not RemClassRegistry.InfoToURI(Info, TypeName, IsScalar) then
          raise EJSONConvertError.CreateFmt(SRemTypeNotRegistered, [Info.Name]);
        S := ConvertEnumToSoap(Info, P, NumIndirect);
        ElemNode := CreateScalarNodeXS(RootNode, Name, TypeName, S);
      end else
      begin
        if NumIndirect > 1 then
          for I := 0 to NumIndirect - 2 do
            P := Pointer(PInteger(P)^);
        TypeTranslator.CastNativeToSoap(Info, S, P, IsNull);
        if IsNull then
          CreateNULLNode(ElemNode, Name)
        else
        begin
          if not RemTypeRegistry.TypeInfoToXSD(Info, TypeName) then
            raise EJSONConvertError.CreateFmt(SRemTypeNotRegistered, [Info.Name]);
          ElemNode := CreateScalarNodeXS(RootNode, Name, TypeName, S);
        end;
      end
    end;
  end;
end;


procedure TJSONConv.ConvertSoapToNativeData(DataP: Pointer; TypeInfo: PTypeInfo;
  Context: TDataContext; RootNode: ISuperObject; Translate, ByRef: Boolean;
  NumIndirect: Integer);
var
  TypeName: InvString;
  IsNull: Boolean;
  Obj: TObject;
  P: Pointer;
  I: Integer;
  ID: InvString;
begin
  IsNull := NodeIsNull(RootNode);
  if TypeInfo.Kind = tkVariant then
  begin
    if NumIndirect > 1 then
      DataP := Pointer(PInteger(DataP)^);
    if IsNull then
    begin
      Variant(PVarData(DataP)^) := NULL;
    end else
      ConvertSoapToVariant(RootNode, DataP);
  end else
  if TypeInfo.Kind = tkDynArray then
  begin
    P := DataP;
    for I := 0 to NumIndirect - 2 do
      P := Pointer(PInteger(P)^);
    P := ConvertSoapToNativeArray(P, TypeInfo, RootNode);
    if NumIndirect = 1 then
      PInteger(DataP)^ := Integer(P)
    else if NumIndirect = 2 then
    begin
      DataP := Pointer(PInteger(DataP)^);
      PInteger(DataP)^ := Integer(P);
    end;
  end else
  if TypeInfo.Kind = tkClass then
  begin
    Obj := ConvertSOAPToObject(RootNode, GetTypeData(TypeInfo).ClassType, TypeName, DataP, NumIndirect);
    if NumIndirect = 1 then
      PTObject(DataP)^ := Obj
    else if NumIndirect = 2 then
    begin
      DataP := Pointer(PInteger(DataP)^);
      PTObject(DataP)^ := Obj;
    end;
  end else
  begin
    if Translate then
    begin
      if NumIndirect > 1 then
        DataP := Pointer(PInteger(DataP)^);
      if not TypeTranslator.CastSoapToNative(TypeInfo, GetNodeAsText(RootNode), DataP, IsNull) then
        raise EJSONConvertError.CreateFmt(STypeMismatchInParam, [RootNode.AsString]);
    end;
  end;
end;


function TJSONConv.ConvertEnumToSoap(Info: PTypeInfo;
  P: Pointer; NumIndirect: Integer): InvString;
var
  Value: Pointer;
  I: Integer;
begin
  Value := P;
  for I := 0 to NumIndirect - 2 do
    Value := Pointer(PInteger(Value)^);
  if NumIndirect = 0 then
    Result := GetEnumName(Info, Byte(Value))
  else
    Result := GetEnumName(Info, PByte(Value)^);
  { NOTE: No need to use SameTypeInfo here since C++ has proper case }
  if Info = TypeInfo(System.Boolean) then
    Result := Lowercase(Result);
end;


function TJSONConv.ConvertSoapToEnum(Info: PTypeInfo; S: InvString;
  IsNull: Boolean): Integer;
begin
  Result := GetEnumValueEx(Info, S);
end;


function TJSONConv.CreateNULLNode(RootNode: ISuperObject; const Name: InvString): ISuperObject;
begin
  Result := TSuperObject.Create(stNull);
  RootNode.O[Name] := Result;
end;


function TJSONConv.NodeIsNULL(Node: ISuperObject): Boolean;
begin
  Result := False;
  if Node = nil then
    Result := True
  else if Node.IsType(stNull) then
    Result := True;
end;

function TJSONConv.ChildNodesAreNull(Node: ISuperObject): Boolean;
var
  I: Integer;
  Child: ISuperObject;
begin
  Result := True;
(*  if Node.ChildNodes.Count > 0 then
  begin
    for I := 0 to Node.ChildNodes.Count-1 do
    begin
      Child := Node.ChildNodes[I];
      if Child.NodeType <> ntElement then
        continue;
      if not NodeIsNull(Node.ChildNodes[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;*)
end;

function TJSONConv.CreateTypedNode(RootNode: ISuperObject;
   const NodeName: WideString; TypeName: WideString; GenPre: Boolean = False): ISuperObject;
begin
  if GenPre then
//    Result := ParentNode.AddChild(NodeName, '', True)
  else
  begin
    { Send no namespace for nested nodes }
//    Result := ParentNode.AddChild(NodeName)
  end;

//  SetNodeType(RootNode, Result, TypeName);
end;

function TJSONConv.CreateScalarNodeXS(RootNode: ISuperObject;
   const NodeName, TypeName: WideString; const Value: WideString; GenPre: Boolean = False): ISuperObject;
begin
  Result := CreateTypedNode(RootNode, NodeName, TypeName);
  RootNode.S[NodeName] := Value;
//  Result.Text := Value;
end;

function TJSONConv.GetOptions: TJSONConvertOptions;
begin
  Result := FOptions;
end;

procedure TJSONConv.SetOptions(const Value: TJSONConvertOptions);
begin
  { NOTE: Some options are mutually exclusive - for example, soDocument
          does not jive well with others. We could provide logic to handle
          this here or we can rely on the caller to know how to set options }
  FOptions := Value;
end;

function TJSONConv.GetNodeAsText(Node: ISuperObject): InvString;
begin
  Result := Node.AsString;
end;

procedure TJSONConv.CheckEncodingStyle(Node: ISuperObject);
begin
end;

procedure TJSONConv.AddObjectAsWriting(Instance: TObject);
var
  I: Integer;
begin
{
  for I := 0 to Length(ObjsWriting) - 1 do
    if ObjsWriting[I] = Instance then
      Exit;
}
  I :=  Length(ObjsWriting);
  SetLength(ObjsWriting, I + 1);
  ObjsWriting[I] := Instance;
end;

function TJSONConv.IsObjectWriting(Instance: TObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(ObjsWriting) -1 do
    if ObjsWriting[I] = Instance then
    begin
      Result := True;
      break;
    end;
end;

procedure TJSONConv.RemoveObjectAsWriting(Instance: TObject);
var
  I, J: Integer;
begin
  I := 0;
  while I < Length(ObjsWriting) do
  begin
    if ObjsWriting[I] = Instance then
      break;
    Inc(I);
  end;
  if I <  Length(ObjsWriting) then
  begin
    for J := I to Length(ObjsWriting)  - 2 do
      ObjsWriting[J] := ObjsWriting[J+1];
    SetLength(ObjsWriting, Length(ObjsWriting) -1);
  end;
end;

procedure TJSONConv.ResetMultiRef;
begin
  SetLength(ObjsWriting, 0);
end;

end.
