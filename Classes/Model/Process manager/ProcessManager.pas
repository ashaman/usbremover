unit ProcessManager;

interface

uses
  Classes, Windows;

type
  TProcessManager = class(TObject)
  private
    function GetSystemInformationTable(TableType: Cardinal): Pointer;
    function GetFileNameFromHandle(Handle: THandle): String;
    function GetDebuggerPrivileges: boolean;
    function GetHandleType: byte;
    constructor Create;
  public
    function GetLockers(Drives: TStringList): TList;
    class function GetInstance: TProcessManager;
  end;

//============================================================================//
implementation

uses
  NTDLL, SysUtils;

type
  TCharArray = array [0..MAX_PATH] of char;

var
  instance: TProcessManager; //singleton variable

//Thread routine
function GetFileName(Parameters: Pointer): Cardinal;
var
  fileNameInfo: FILE_NAME_INFORMATION; //file name info
  ioStatusBlock: IO_STATUS_BLOCK; //I/O status block
  threadParameters: TGetFileNameThreadParam; //thread parameter structure
  objectInfo: TOBJECT_NAME_INFORMATION; //object info
  dummy: Cardinal;
begin
  //initialization
  threadParameters := PGetFileNameThreadParam(Parameters)^;
  ZeroMemory(@fileNameInfo, sizeof(fileNameInfo));
  Result := 0;
  Result := NtQueryInformationFile(threadParameters.hFile, @ioStatusBlock,
    @fileNameInfo, 2*MAX_PATH, FileNameInformation);
  if Result = STATUS_SUCCESS
  then begin
    Result := NtQueryObject(threadParameters.hFile, ObjectNameInformation,
      @objectInfo, 2*MAX_PATH, @dummy);
    if Result = STATUS_SUCCESS
    then begin
      threadParameters.Status := Result;
      WideCharToMultiByte(CP_ACP, 0,
        @objectInfo.Name.Buffer[objectInfo.Name.MaximumLength - objectInfo.Name.Length],
        objectInfo.Name.Length, PChar(@threadParameters.Data[0]), MAX_PATH,
        nil, nil);
    end //then - status indicates successful ending of operation
    else begin
      Result := STATUS_SUCCESS;
      threadParameters.Status := Result;
      WideCharToMultiByte(CP_ACP, 0, PWideChar(@fileNameInfo.FileName[0]),
        ioStatusBlock.Information, PChar(@threadParameters.Data[0]), MAX_PATH,
        nil, nil);
    end; //else - other statuses
  end; //then - successfull call
  //returning parameters
  PGetFileNameThreadParam(Parameters)^ := threadParameters;
  ExitThread(Result);
end; //GetFileName

//This function gets file name from the handle
function TProcessManager.GetFileNameFromHandle(Handle: THandle): String;
begin

end; //GetFileNameFromFileHandle

//This function gets system information table
function TProcessManager.GetSystemInformationTable(TableType: Cardinal): Pointer;
var
  buffer: Pointer; //buffer for function call
  status: Cardinal; //function status
  bufferSize: Cardinal; //buffer size
begin
  Result := nil;
  bufferSize := Cardinal(-1);
  repeat
    buffer := AllocMem(bufferSize);
    //using ZwQuerySystemInformation
    status := ZwQuerySystemInformation(TableType, buffer, bufferSize, nil);
    if status = STATUS_INFO_LENGTH_MISMATCH
    then begin
      FreeMem(buffer, bufferSize);
      bufferSize := bufferSize*2;
    end; //then - buffer is too little
  until status <> STATUS_INFO_LENGTH_MISMATCH;
end; //GetSystemInformationTable


function TProcessManager.GetLockers(Drives: TStringList): TList;
begin
  Result := TList.Create;
end; //GetLockers

//This function gets file handle type using the NUL device
function TProcessManager.GetHandleType: byte;
var
  handle: THandle; //NUL device handle
  systemHandleInfo: PSYSTEM_HANDLE_INFORMATION_EX; //system handle information
  i: integer; //loop index
begin
  Result := 0;
  handle := CreateFile(PChar('NUL'), GENERIC_READ, FILE_SHARE_READ or
    FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if handle = INVALID_HANDLE_VALUE
  then begin
    raise Exception.Create(SysErrorMessage(GetLastError));
  end //then - raise exception
  else begin
    try
      //getting handle tables
      systemHandleInfo := GetSystemInformationTable(SystemHandleInformation);
      if not Assigned(systemHandleInfo)
      then begin
        raise Exception.Create('NullPointer exception');
      end //then - not assigned
      else begin
        try
          //FOR-LOOP
          for i := 0 to systemHandleInfo^.NumberOfHandles-1 do
          begin
            //comparing file handles to get current process
            if systemHandleInfo^.Information[i].Handle = handle
            then begin
              if systemHandleInfo^.Information[i].ProcessId = GetCurrentProcessId
              then begin
                Result := systemHandleInfo^.Information[i].ObjectTypeNumber;
                break;
              end; //then
            end; //then
          end; //for-loop
        finally
          FreeMem(systemHandleInfo);
        end; //finally - got result
      end; //else - successfully got
    finally
      CloseHandle(handle);
    end; //finally
  end; //else - getting info table and saving NUL
end; //GetHandleType

class function TProcessManager.GetInstance;
begin
  if not Assigned(instance)
  then begin
    instance := TProcessManager.Create;
  end; //singleton initialization
  Result := instance;
end; //GetInstance

function TProcessManager.GetDebuggerPrivileges;
var
  handle: THandle; //process handle
  privileges: TTokenPrivileges; //token privileges
begin
  Result := false;
  //here we get debugger privileges
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
    handle)
  then begin
    raise Exception.Create('Cannot adjust debugger privileges!');
  end
  else begin
    if LookupPrivilegeValue(nil, PChar('SeDebugPrivilege'),
      privileges.Privileges[0].Luid)
    then begin
      privileges.PrivilegeCount := 1;
      privileges.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      Result := AdjustTokenPrivileges(handle, false, privileges, 0,
        PTokenPrivileges(nil)^, PDWORD(nil)^);
    end; //then - lookup privileges
  end; //else - OpenProcessToken
end;

constructor TProcessManager.Create;
begin
  GetDebuggerPrivileges;
end; //Create

end.
