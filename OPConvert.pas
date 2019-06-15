{*******************************************************}
{                                                       }
{ Borland Delphi Visual Component Library               }
{                 SOAP Support                          }
{                                                       }
{ Copyright (c) 2001 Borland Software Corporation       }
{                                                       }
{*******************************************************}

unit OPConvert;

interface

uses IntfInfo, InvokeRegistry, SysUtils, Classes;

type

  { Various options that control how data is [de]serialized. }
  TJSONConvertOption = (soPretty,
                        soXXXXHdr);

  TJSONConvertOptions = set of TJSONConvertOption;

  IOPConvert = interface
  ['{1F955FE3-890B-474C-A3A4-5E072D30CC4F}']
    { Property Accessors }
    function  GetOptions: TJSONConvertOptions;
    procedure SetOptions(const Value: TJSONConvertOptions);
    function  GetTempDir: string;
    procedure SetTempDir(const Value: string);

    { client methods }
    function InvContextToMsg(const IntfMD: TIntfMetaData;
                             MethNum: Integer;
                             Con: TInvContext;
                             Headers: THeaderList): TStream;
    procedure ProcessResponse(const Resp: TStream;
                              const IntfMD: TIntfMetaData;
                              const MD: TIntfMethEntry;
                              Context: TInvContext;
                              Headers: THeaderList);
    { server methods }
    procedure MsgToInvContext(const Request: InvString;
                              const IntfMD: TIntfMetaData;
                              var MethNum: Integer;
                              Context: TInvContext); overload;
    procedure MsgToInvContext(const Request: TStream;
                              const IntfMD: TIntfMetaData;
                              var MethNum: Integer;
                              Context: TInvContext;
                              Headers: THeaderList);  overload;
    procedure MakeResponse(const IntfMD: TIntfMetaData;
                              const MethNum: Integer;
                              Context: TInvContext;
                              Response: TStream;
                              Headers: THeaderList);
    procedure MakeFault(const Ex: Exception; EStream: TStream);

    property Options:  TJSONConvertOptions read GetOptions write SetOptions;
    property TempDir: string read GetTempDir write SetTempDir;
  end;


implementation

end.
