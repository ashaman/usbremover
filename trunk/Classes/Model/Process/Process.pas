{
  Process data class
  Contains data about the process and files opened by it
  Developed by J.L. Blackrow
}
unit Process;

interface

uses
  Classes;

type
  TProcess = class(TObject)
  private
    fHandle: THandle;
    fName: string;
    fOpenedFiles: TStringList;
  public
    constructor Create(Handle: THandle; ProcessName: string);
    destructor Destroy; override;
    property Handle: THandle read fHandle;
    property Name: string read fName;
    property OpenedFiles: TStringList read fOpenedFiles;
end;

implementation

{CONSTRUCTOR}
constructor TProcess.Create(Handle: THandle; ProcessName: String);
begin
  inherited Create;
  fHandle := Handle;
  fName := ProcessName;
  fOpenedFiles := TStringList.Create;
end; //Create

{DESTRUCTOR}
destructor TProcess.Destroy;
begin
  fOpenedFiles.Free;
  inherited Destroy;
end; //Destroy

end.
