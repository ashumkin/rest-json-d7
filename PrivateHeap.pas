{*******************************************************}
{                                                       }
{ Borland Delphi Visual Component Library               }
{ Private Heap Class                                    }
{                                                       }
{ Copyright (c) 2005 Borland Software Corporation       }
{ Original implementation by Hallvard Vassbotn          }
{                                                       }
{*******************************************************}

unit PrivateHeap;

interface

uses
  Windows;

type
  // The TPrivateHeap class gives basic memory allocation capability using the
  // Windows Heap API's.
  TPrivateHeap = class(TObject)
  private
    FHandle: THandle;
    FAllocationFlags: DWORD;
    FPageFlags: DWORD;
    function GetHandle: THandle;
  public
    constructor Create(APageFlags: DWORD = 0; ASerialized: Boolean = True);
    destructor Destroy; override;
    procedure GetMem(var P{: pointer}; Size: DWORD); virtual;
    procedure FreeMem(P: pointer);
    function SizeOfMem(P: pointer): DWORD;
    property Handle: THandle read GetHandle;
  end;

function CodeHeap: TPrivateHeap;

implementation

uses
  SysUtils;

var
  FCodeHeap: TPrivateHeap;

function CodeHeap: TPrivateHeap;
var
  _CodeHeap: TPrivateHeap;
begin
  if FCodeHeap = nil then begin
    _CodeHeap := TPrivateHeap.Create(PAGE_EXECUTE_READWRITE);
{$IF CompilerVersion <= 15.0}
    if InterlockedCompareExchange(Pointer(FCodeHeap), Pointer(_CodeHeap), nil) <> nil then
{$ELSE}
    if InterlockedCompareExchange(Integer(FCodeHeap), Integer(_CodeHeap), 0) <> 0 then
{$IFEND}
      _CodeHeap.Free;
  end;
  Result := FCodeHeap;
end;

{ TPrivateHeap }

constructor TPrivateHeap.Create(APageFlags: DWORD = 0; ASerialized: boolean = True);
{$IF CompilerVersion <= 15.0}
const
  HEAP_NO_SERIALIZE = 0;
{$IFEND}
begin
  inherited Create;
  FPageFlags := APageFlags;
  if not ASerialized then
    FAllocationFlags := HEAP_NO_SERIALIZE;
end;

destructor TPrivateHeap.Destroy;
begin
  if FHandle <> 0 then
  begin
    if not Windows.HeapDestroy(FHandle) then
      RaiseLastOSError;
    FHandle := 0;
  end;
  inherited Destroy;
end;

procedure TPrivateHeap.FreeMem(P: pointer);
begin
  if not Windows.HeapFree(Handle, 0, P) then
    RaiseLastOSError;
end;

function TPrivateHeap.GetHandle: THandle; begin
  if FHandle = 0 then
  begin
    FHandle := Windows.HeapCreate(0, 0, 0);
    if FHandle = 0 then
      RaiseLastOSError;
  end;
  Result := FHandle;
end;

procedure TPrivateHeap.GetMem(var P{: pointer}; Size: DWORD);
var
  ExistingFlags: DWORD;
begin
  Pointer(P) := Windows.HeapAlloc(Handle, FAllocationFlags, Size);
  if Pointer(P) = nil then
    RaiseLastOSError;
  if FPageFlags <> 0 then
    if not Windows.VirtualProtect(Pointer(P), Size, FPageFlags, @ExistingFlags) then
      RaiseLastOSError;
end;

function TPrivateHeap.SizeOfMem(P: pointer): DWORD;
begin
  Result := Windows.HeapSize(Handle, 0, P);
  // HeapSize does not set GetLastError, but returns $FFFFFFFF if it fails
  if Result = $FFFFFFFF then
    Result := 0;
end;

initialization
finalization
  FreeAndNil(FCodeHeap);
end.
