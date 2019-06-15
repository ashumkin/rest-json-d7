{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{         SOAP Attachment                               }
{                                                       }
{       Copyright (c) 2001 Inprise Corporation          }
{                                                       }
{*******************************************************}
unit SOAPAttach;

interface

uses
  SysUtils, Types, Classes, InvokeRegistry,
  HTTPApp;
const
  EOL           = #13#10;             { Linux vs. Windows is not relevant }
  BlockReadSize = 10240;              { Buffer side reading stream blocks }

type

  TSOAPAttachmentData = class(TSOAPAttachment)
  private
    FID: string;
  public
    { Id used to identify Attachment: Content-Id or Content-Location }
    property ID: string read FID write FID;
    procedure SetSourceStream(const Value: TStream; const Ownership: TStreamOwnership = soOwned); override;
    { allow Filename to be set without clearing out SourceStream }
    procedure SetCacheFile(const Value: string);
  end;

{ treats a TWebRequest as a TStream }

  TWebRequestStream = class(TStream)
  private
    FWebRequest: TWebRequest;
    FPosition: Int64;
    FSize: Int64;
    FContentSize: Integer;
    FSavedChars: string;
    FContentType: string;
    FMaxLine: Integer;
  public
    constructor Create(ARequest: TWebRequest); reintroduce;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadLn: String;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property MaxLine: Integer read FMaxLine write FMaxLine;
  end;

{ Utility functions }

  function GetTempHandle(var ATempFileName: string): THandle;
  function GetTempDir: string;
  function GetMimeBoundaryFromType(const ContentType: string): string;
  function GetBorlandMimeContentType: string;

implementation

uses Math, RESTJSONConst, {$IFDEF LINUX}Libc{$ENDIF}{$IFDEF MSWINDOWS}Windows{$ENDIF};

{ Utility functions }

  function GetTempDir: string;
  begin;
{$IFDEF LINUX}
    Result := GetEnv('TMPDIR');
    if Result = '' then
      Result := '/tmp/'
    else if Result[Length(Result)] <> PathDelim then
      Result := Result + PathDelim;
{$ENDIF}
{$IFDEF MSWINDOWS}
    SetLength(Result, 255);
    SetLength(Result, GetTempPath(255, (PChar(Result))));
{$ENDIF}
  end;

  function GetTempHandle(var ATempFileName: string): THandle;
{$IFDEF MSWINDOWS}
  var
    Index: Integer;
    AFileName: string;
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    Index := 0;
    AFileName := ATempFileName + IntToStr(Index);
    while FileExists(AFileName) do
    begin
      Inc(Index);
      AFileName := ATempFileName + IntToStr(Index);
    end;
    ATempFileName := AFileName;
    Result := FileCreate(AFileName);
{$ENDIF}
{$IFDEF LINUX}
    UniqueString(ATempFileName);
    Result := mkstemp(Pointer(ATempFileName));
{$ENDIF}
    if Integer(Result) < 1 then
      raise Exception.Create(STempFileAccessError);
  end;

  function GetBorlandMimeContentType: string;
  begin
    Result := Format(ContentHeaderMime, [SBorlandMimeBoundary]) +
                          Format(SStart, [SBorlandSoapStart]);
  end;

  function GetMimeBoundaryFromType(const ContentType: string): string;
  begin
    { As per rfc2112 - http://www.faqs.org/rfcs/rfc2112.html -
      we expect a content-type 'Multipart/Related' }
    if Pos(SMultipartRelated, LowerCase(ContentType)) = 1 then                        { do not localize }
    begin
      Result := Copy(ContentType, Pos(SBoundary, ContentType) + Length(SBoundary), MaxInt);
      if Pos(';', Result) > 1 then
        Result := Copy(Result, 1, Pos(';', Result) -1);
    end else
      Result := '';
  end;

{ TSOAPAttachmentData }

procedure TSOAPAttachmentData.SetSourceStream(const Value: TStream; const Ownership: TStreamOwnership = soOwned);
begin
  InternalSetSourceStream(Value, Ownership);
end;

procedure TSOAPAttachmentData.SetCacheFile(const Value: string);
begin
  SetSourceFile('');
  InternalSetCacheFile(Value);
  CacheFilePersist := True;
end;

{ TWebRequestStream }

constructor TWebRequestStream.Create(ARequest: TWebRequest);
begin
  inherited Create;
  FWebRequest := ARequest;
  FSize := FWebRequest.ContentLength;
  FPosition := 0;
  FContentSize := Length(FWebRequest.Content);
  FContentType := FWebRequest.ContentType;
  FSavedChars := '';
  FMaxLine := BlockReadSize;
end;

destructor TWebRequestStream.Destroy;
begin
  inherited;
end;

{ assumes user knows headers are to follow, followed by blank line }
function TWebRequestStream.ReadLn: string;
var
  ReadCount, CRPos: Integer;
  SplitLF: string;
begin
  SetLength(Result, MaxLine);
  ReadCount := Read(Result[1], MaxLine);
  SetLength(Result, ReadCount);
  CrPos := Pos(EOL, Result);
  if CrPos > 0 then
  begin
    Inc(CrPos);
    FSavedChars := Copy(Result, CrPos + 1, Length(Result) - CrPos) + FSavedChars;
    SetLength(Result, CrPos);
  end else
  begin
    { Check for split EOL }
    if (Length(Result) > 0 ) and (Result[Length(Result)] = #13) then
    begin
      SetLength(SplitLF, 1);
      Read(SplitLF[1], 1 );
      if SplitLF[1] = #10 then
      begin
        { cut off #13 from result }
        SetLength(Result, MaxLine -1);
        FSavedChars := FSavedChars + EOL;
      end;
    end;
  end;
end;

function TWebRequestStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesToRead, BytesRead, SaveStart: LongInt;
  P: PChar;

  procedure LoadSavedChars(SaveStart: Integer);
  var
    Buffer : string;
  begin
    if FPosition < FContentSize then
    begin
      { read first from FWebRequest.Content buffer }
      BytesToRead := Min(Count, FContentSize - FPosition);
      SetLength(Buffer, BytesToRead);
      Move(FWebRequest.Content[FPosition + 1], Buffer[1], BytesToRead);
      FSavedChars := FSavedChars + Buffer;
      Inc(FPosition, BytesToRead);
      Inc(SaveStart, BytesToRead);
    end;
    if SaveStart < Count then
    begin
      { if still missing bytes then use TWebRequest.ReadClient }
      while (SaveStart < Count) and (FPosition < FSize) do
      begin
        BytesToRead := Min(Count - SaveStart, FSize - FPosition);
        SetLength(Buffer, BytesToRead);
        BytesRead := FWebRequest.ReadClient(Buffer[1], BytesToRead);
        if BytesRead < 1 then
          break;
        SetLength(Buffer, BytesRead);
        FSavedChars := FSavedChars + Buffer;
        Inc(FPosition, BytesRead);
        Inc(SaveStart, BytesRead);
      end;
    end;
  end;

begin
  if Assigned(FWebRequest) then
  begin
    SaveStart := Length(FSavedChars);
    if SaveStart < Count then
      LoadSavedChars(SaveStart);
    P := @Buffer;
    Result := 0;
    { retrieve from Saved Buffer }
    BytesToRead := Min(Count, Length(FSavedChars));
    Move(FSavedChars[1], P[Result], BytesToRead);
    Inc(Result, BytesToRead);
    if BytesToRead >= Length(FSavedChars) then
      FSavedChars := ''
    else
      FSavedChars := Copy(FSavedChars, BytesToRead + 1, MaxInt);
  end else
    Result := 0;
end;

function TWebRequestStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset + Length(FSavedChars);
    soFromEnd: FPosition := FSize + Length(FSavedChars);
    soFromCurrent: Inc(FPosition, Offset);
  end;
  Result := FPosition - Length(FSavedChars);
end;

function TWebRequestStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create(SMethodNotSupported);
end;

end.
