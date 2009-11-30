unit Process;

interface

uses
  Classes, WbemScripting_TLB, OleServer, ActiveX, NTdll;

type
  TProcess = class(TObject)
  private
    p_name: string;
    //execPath: string;
    p_handle: integer;
    p_files: TList;
  public
    constructor Create(g_name: string; g_handle: integer); overload;
    constructor Create(); overload;
    destructor Destroy; override;
    property Name: string read p_name write p_name;
    property Handle: integer read p_handle write p_handle;
    property Files: TList read p_files write p_files;
end;

implementation
constructor TProcess.Create(g_name: string; g_handle: integer);
begin
  name := g_name;
  handle := g_handle;
  inherited Create;
end;

constructor TProcess.Create;
begin
  name := '';
  handle := -1;
  inherited Create;
end;

destructor TProcess.Destroy;
begin
  inherited Destroy;
end;

end.
