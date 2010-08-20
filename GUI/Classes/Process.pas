{
    Process.pas
    Started: 19.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: (L)GPL v3(?) /EULA

    Defines process class
}
unit Process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Communication;

type
    {
        Description:
            This class encapsulates the process data
    }
    TProcess = class(TObject)
    private
        fOpenedFiles: TStrings; //opened files
        fProcessId: DWORD; //process identifier
        fProcessName: WideString; //process name

        function GetFileCount: integer; //gets total files count
        function GetFileName(index: integer): String; //indexer procedure

    public
        constructor Create (procInfo: PROC_INFO); //constructor
        destructor Destroy; override; //destructor

        procedure AddFileName(fileName: TWideCharArray); //adds a new file name
        property FileCount: integer read GetFileCount; //gets file count
        property OpenedFiles[index: integer]: String read GetFileName; //files

    end;

implementation

{
    Purpose:
        Gets the total count of files locked by this process
    Parameters:
        None
    Return value:
        Total count of locked files
}
function TProcess.GetFileCount: integer;
begin
    Result := Self.fOpenedFiles.Count;
end; //GetFileCount

{
    Purpose:
        Indexer function
    Parameters:
        index - index of the string to get
    Return value:
        String with index [index]
}
function TProcess.GetFileName(index: integer): String;
begin
    Result := fOpenedFiles.Strings[index];
end; //GetFileName

{
    Purpose:
        Adds a new file name to the list of blocked files names
    Parameters:
        fileName - a file name to add
    Return value:
        None
}
procedure TProcess.AddFileName(fileName: TWideCharArray);
begin
    Self.fOpenedFiles.Append(Trim(fileName));
end; //AddFileName

{
    Purpose:
        Constructor. Initializes a new instance of TProcess using
        PROC_INFO structure data
    Parameters:
        procInfo - PROC_INFO structure with the process information
}
constructor TProcess.Create(procInfo: PROC_INFO);
begin
    inherited Create;
    self.fOpenedFiles := TStringList.Create;
    //saving name and id info
    self.fProcessId := procInfo.dwId;
    self.fProcessName := procInfo.name;
end; //constructor

{
    Purpose:
        Destructor
}
destructor TProcess.Destroy;
begin
    //clearing internal fields
    fOpenedFiles.Clear;
    fOpenedFiles.Destroy;
    inherited Destroy;
end; //destructor

end.

