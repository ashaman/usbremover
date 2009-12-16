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
    fProcesses: TList;
    procedure AddProcessFilePair(ProcessName: String; ProcessHandle: THandle; FileName: String);
    function AssociateMountPointsAndDrives(Points: TStringList): TStringList;
    function GetSystemInformationTable(TableType: Cardinal): Pointer;
    function GetFileNameFromHandle(Handle: THandle): String;
    function GetDebuggerPrivileges: boolean;
    function GetHandleType: byte;
    constructor Create;
  public
    function GetLockers(Drives: TStringList; ProgressCallback: TNotifyEvent): TList;
    class function GetInstance: TProcessManager;
    property BlockerProcesses: TList read fProcesses;
  end;

//============================================================================//
implementation

uses
  NTDll, SysUtils, Forms, WinIOCtl, Process;

var
  instance: TProcessManager; //singleton variable

//This procedure associates drive mount points and physical drives
function TProcessManager.AssociateMountPointsAndDrives(Points: TStringList): TStringList;
var
  i: integer; //loop counter
  buf: TCharArray; //buffer
  s: string; //buffer string
begin
  Result := TStringList.Create;
  for i := 0 to Points.Count-1 do
  begin
    s := Points.Strings[i];
    Delete(s, Length(s), 1);
    if QueryDosDevice(PChar(@s[1]), PChar(@buf[0]), sizeof(buf)) <> 0
    then begin
      Result.AddObject(Points.Strings[i],TObject(Trim(String(buf))));
    end; //then - device alias
  end; //loop
  //Points.Free;
end; //AssociateMountPointsAndDrives

//This function adds process-file pair to the processes list
procedure TProcessManager.AddProcessFilePair(ProcessName: String;
  ProcessHandle: THandle; FileName: String);
var
  i: integer; //loop index
  found: boolean; //boolean flag
  tempProcess: TProcess; //temporary value for the process
begin
  i := 0;
  found := false;
  //searching process by its name
  while (i <= fProcesses.Count-1) and (not found) do
  begin
    tempProcess := TProcess(fProcesses.Items[i]);
    //if the process was found, we try to find the specified file name
    if tempProcess.Name = ProcessName
    then begin
      found := true;
      tempProcess.OpenedFiles.Add(FileName);
    end //then - process was found
    else begin
      Inc(i);
    end; //else - searching
  end; //while
  if not found
  then begin
    //adding new process to the process list
    tempProcess := TProcess.Create(ProcessHandle, ProcessName);
    tempProcess.OpenedFiles.Add(FileName);
    fProcesses.Add(tempProcess);
  end; //then - process was not in the process list, created
end; //AddProcessFilePair

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
      threadParameters.Status := STATUS_SUCCESS;
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
//Its implementation uses threads because some of files in system are pipes,
//and when attached to a named pipe, your process may "hang" waiting for the
//pipe to be filled. So, we run the GetFileName function in a separate thread
//and check it after 200 milliseconds. If its work has finished - all is OK,
//otherwise, we kill the thread and exit this function
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
      //starting thread and waiting until it finishes its work or it's
      //out of time
      case WaitForSingleObject(thread, 100) of
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
//Type of table is specified using the TableType parameter
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
  i, j: integer; //loop index
  processHandle: THandle; //process handle
  fileHandle: THandle; //file handle
  filePath: string; //file path
  processName: string; //blocker name
  tmpSysInfo: PSYSTEM_PROCESS_INFORMATION; //temporary pointer
  drivePos: integer; //position of drive string
  buf: string; //buffer string
  tmpDrives: TStringList;
begin
  //getting file type on the current system
  fileType := GetHandleType;
  //getting system information about processes and threads
  tmpDrives := AssociateMountPointsAndDrives(Drives);
  systemInformation := GetSystemInformationTable(SystemProcessesAndThreadsInformation);
  if systemInformation = nil
  then begin
    raise Exception.Create('Failed to get information about processes');
  end //then
  else begin
    try
      //getting system
      handleInformation := GetSystemInformationTable(SystemHandleInformation);
      if handleInformation = nil
      then begin
        raise Exception.Create('Failed to get information about handles');
      end //then
      else begin
        try
          //loop over all the handles in system
          for i := 0 to handleInformation^.NumberOfHandles-1 do
          begin
            //in this handle is a file handle
            if handleInformation^.Information[i].ObjectTypeNumber = fileType
            then begin
              //we duplicate the handle of the owning process
              processHandle := OpenProcess(PROCESS_DUP_HANDLE, true,
                handleInformation^.Information[i].ProcessId);
              //if it was successful...
              if processHandle > 0
              then begin
                try
                  //then we duplicate the handles
                  if DuplicateHandle(processHandle,
                    handleInformation^.Information[i].Handle, //FIXED BUG!!!
                    GetCurrentProcess, @fileHandle, 0, false,
                    DUPLICATE_SAME_ACCESS)
                  then begin
                    //if the previous operation was successful...
                    try
                      //we get file name from the handle
                      filePath := GetFileNameFromHandle(fileHandle);
                      //and if it's not empty...
                      if filePath <> ''
                      then begin
                        for j := 0 to tmpDrives.Count-1 do
                        begin
                          buf := String(tmpDrives.Objects[j]);
                          drivePos := Pos(buf, filePath);
                          if drivePos = 1
                          then begin
                            Delete(filePath, drivePos, Length(buf)+1);
                            filePath := tmpDrives.Strings[j]+filePath;
                            //we try to find its blocker
                            tmpSysInfo := systemInformation;
                            while (tmpSysInfo^.NextOffset <> 0) do
                            begin
                              //if handles are matching...
                              if tmpSysInfo^.ProcessID =
                                handleInformation^.Information[i].ProcessId
                              then begin
                                //voila! we found the blocker
                                processName := tmpSysInfo^.ModuleName;
                                //and we add it to the list
                                AddProcessFilePair(processName,
                                  tmpSysInfo^.ProcessID, filePath);
                                break;
                              end; //then - we found file's blocker
                              tmpSysInfo := Pointer(Cardinal(tmpSysInfo)
                                + tmpSysInfo^.NextOffset);
                            end; //while
                            break;
                          end; //then - found match
                        end; //loop - j - find device
                      end; //then - file path is not empty
                    finally
                      CloseHandle(fileHandle);
                    end; //finally - closing file handle
                  end; //then - process handle duplicated
                finally
                  CloseHandle(processHandle);
                end; //finally
              end; //then - opened process
            end; //then - handle is file handle
            Application.ProcessMessages;
            //progress callback
            if Assigned(ProgressCallback)
            then begin
              ProgressCallback(TObject(Round(100*i/handleInformation^.NumberOfHandles)));
            end; //then - callback call
          end; //for-loop
        finally
          FreeMem(handleInformation, sizeof(SYSTEM_HANDLE_INFORMATION_EX));
        end; //finally - free handle information memory
      end; //else - successfully got handle information
    finally
      FreeMem(systemInformation, sizeof(SYSTEM_PROCESS_INFORMATION));
    end; //finally - free processes information memory
  end; //else - successfully got processes information
  //Drives.Free;
  //tmpDrives.Free;
  Result := fProcesses;
end; //GetLockers

//This function gets file handle type using the NUL device
//It solves the first problem - getting file type on different systems
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

{SINGLETON METHOD}
class function TProcessManager.GetInstance;
begin
  if not Assigned(instance)
  then begin
    instance := TProcessManager.Create;
  end; //singleton initialization
  Result := instance;
end; //GetInstance

//This function gets the debugger privileges in order to allow this process to
//attach to others
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
  end //then - failed to get current process
  else begin
    if LookupPrivilegeValue(nil, PChar('SeDebugPrivilege'),
      privileges.Privileges[0].Luid)
    then begin
      privileges.PrivilegeCount := 1;
      privileges.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      Result := AdjustTokenPrivileges(handle, false, privileges, 0,
        PTokenPrivileges(nil)^, PDWORD(nil)^);
    end; //then - lookup privileges and setting them to the appropriate state
  end; //else - OpenProcessToken
end; //GetDebuggerPrivileges

{CONSTRUCTOR}
constructor TProcessManager.Create;
begin
  inherited Create;
  fProcesses := TList.Create;
  GetDebuggerPrivileges;
end; //Create

end.
