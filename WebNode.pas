{***************************************************************}
{                                                               }
{   Borland Delphi Visual Component Library                     }
{                                                               }
{   Copyright (c) 2000-2001 Borland Software Corporation        }
{                                                               }
{***************************************************************}

unit WebNode;

interface

uses Classes, IntfInfo;

type



  { IWebNode defines the basic interface to be implemented by any
    SOAP Transport. IOW, if you want to implement SOAP on SMTP,
    plain Socket, etc - you'll have to create a RIO that implements
    IWebNode.
    See THTTPRIO and TLinkedRIO for examples }
  IWebNode = interface
  ['{77DB2644-0C12-4C0A-920E-89579DB9CC16}']
    { Obsolete - use version that takes a Stream as first parameter }
    procedure BeforeExecute(const IntfMD: TIntfMetaData;
                            const MethMD: TIntfMethEntry;
                            MethodIndex: Integer);

    procedure Execute(const Request: TStream; Response: TStream);
  end;


implementation

end.
