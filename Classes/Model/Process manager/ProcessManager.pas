{
  Process manager class.
  Made as singleton
  Ported from the C code published on WWW.WASM.RU
}
unit ProcessManager;

interface

uses
  Classes, Windows;

type
  //Process manager class
  TProcessManager = class(TObject)
  private
    function GetSystemInformationTable(TableType: Cardinal): Pointer;
    function GetFileNameFromHandle(Handle: THandle): String;
    function GetDebuggerPrivileges: boolean;
    function GetHandleType: byte;
    constructor Create;
  public
    function GetLockers(Drives: TStringList; ProgressCallback: TNotifyEvent): TList;
    class function GetInstance: TProcessManager;
  end;

//============================================================================//
implementation

uses
  NTDLL, SysUtils, Forms;

type
  TCharArray = array [0..MAX_PATH] of char;

var
  instance: TProcessManager; //singleton variable

//Thread routine. Called by the thread in TProcessManager.GetFileNameFromHandle
function GetFileName(Parameters: Pointer): Cardinal; stdcall;
var
  fileNameInfo: FILE_NAME_INFORMATION; //file name info
  ioStatusBlock: IO_STATUS_BLOCK; //I/O status block
  threadParameters: TGetFileNameThreadParam; //thread parameter structure
  objectInfo: TOBJECT_NAME_INFORMATION; //object info
  dummy: Cardinal;
begin
  //initialization
  threadParameters := TGetFileNameThreadParam(Parameters^);
  ZeroMemory(@fileNameInfo, sizeof(fileNameInfo));
  //query for the handle information - because hanlde may not point to the file
  Result := NtQueryInformationFile(threadParameters.hFile, @ioStatusBlock,
    @fileNameInfo, 2*MAX_PATH, FileNameInformation);
  if Result = STATUS_SUCCESS
  then begin
    //secont query for the NT Query Object
    Result := NtQueryObject(threadParameters.hFile, ObjectNameInformation,
      @objectInfo, 2*MAX_PATH, @dummy);
    if Result = STATUS_SUCCESS
    then begin
      threadParameters.Status := Result;
      //converting PWideChar to TCharArray
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
  end; //then - successfully got info about the file handle
  //returning parameters
  PGetFileNameThreadParam(Parameters)^ := threadParameters;
  ExitThread(Result);
end; //GetFileName

//This function gets file name from the handle
function TProcessManager.GetFileNameFromHandle(Handle: THandle): String;
var
  thread: THandle; //thread handle;
  parameters: TGetFileNameThreadParam; //thread parameters
  exitCode: Cardinal; //thread exit code
  threadId: Cardinal; //therad id
begin
  Result := '';
  ZeroMemory(@parameters, sizeof(parameters));
  parameters.hFile := Handle;
  thread := CreateThread(nil, 0, @GetFileName, @parameters, 0, threadId);
  if thread = INVALID_HANDLE_VALUE
  then begin
  end //then - creation failed
  else begin
    try
      case WaitForSingleObject(thread, 200) of
        WAIT_OBJECT_0:
          begin
            GetExitCodeThread(thread, exitCode);
            if exitCode = STATUS_SUCCESS
            then begin
              Result := parameters.Data;
            end; //successfully
          end; //WAIT
        WAIT_TIMEOUT:
          begin
            TerminateThread(handle, 0);
          end; //TIMEOUT
      end; //case
    finally
      CloseHandle(thread);
    end; //finally
  end; //else - thread created
end; //GetFileNameFromFileHandle

//This function gets system information table
function TProcessManager.GetSystemInformationTable(TableType: Cardinal): Pointer;
var
  buffer: Pointer; //buffer for function call
  status: Cardinal; //function status
  bufferSize: Cardinal; //buffer size
begin
  bufferSize := 1;
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
  Result := buffer;
end; //GetSystemInformationTable

//This function gets all the processes blocking files on the flash and
//returns the list op TProcess values
//If the callback is specified, this function notifies aboout the percentage of
//processed work
function TProcessManager.GetLockers(Drives: TStringList;
  ProgressCallback: TNotifyEvent): TList;
var
  fileType: Cardinal; //file type on NT OS
  systemInformation: PSYSTEM_PROCESS_INFORMATION; //process information
  handleInformation: PSYSTEM_HANDLE_INFORMATION_EX; //system handle information
  i: integer; //loop index
  processHandle: THandle; //process handle
  fileHandle: THandle; //file handle
  filePath: string; //file path
begin
  Result := TList.Create;
  //getting file type on the current system
  fileType := GetHandleType;
  //getting system information about processes and threads
  systemInformation := GetSystemInformationTable(SystemProcessesAndThreadsInformation);
  if systemInformation = nil
  then begin
    raise Exception.Create('OLOLO');
  end //then
  else begin
    try
      //getting system 
      handleInformation := GetSystemInformationTable(SystemHandleInformation);
      if handleInformation = nil
      then begin
        raise Exception.Create('OLOLO! OLOLO!');
      end //then
      else begin
        try
          for i := 0 to handleInformation^.NumberOfHandles-1 do
          begin
            if handleInformation^.Information[i].ObjectTypeNumber = fileType
            then begin
              processHandle := OpenProcess(PROCESS_DUP_HANDLE, true,
                handleInformation^.Information[i].ProcessId);
              if processHandle > 0
              then begin
                try
                  if DuplicateHandle(processHandle,
                    handleInformation^.Information[i].ProcessId,
                    GetCurrentProcess, @fileHandle, 0, false,
                    DUPLICATE_SAME_ACCESS)
                  then begin
                    try
                      filePath := GetFileNameFromHandle(fileHandle);
                      filePath := filePath + 'ololo';
                    finally
                      CloseHandle(fileHandle);
                    end; //finally
                  end; //then - process handle duplicated
                finally
                  CloseHandle(processHandle);
                end; //finally
              end; //then - opened process
            end; //then - handle is process
            Application.ProcessMessages;
            if Assigned(ProgressCallback)
            then begin
              ProgressCallback(TObject(Round(100*i/handleInformation^.NumberOfHandles)));
            end; //then - callback call
          end; //for-loop
        finally
          FreeMem(handleInformation);
        end; //finally
      end; //else
    finally
      FreeMem(systemInformation);
    end; //finally
  end; //else
end; //GetLockers

//This function gets file handle type using the NUL device
//The first problem
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
        raise Exception.Create(SysErrorMessage(GetLastError));
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
  if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
    handle)
  then begin
    raise Exception.Create(SysErrorMessage(GetLastError));
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
