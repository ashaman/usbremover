unit ProcessManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, NTdll, Process;

type
TProcessManager = class(TObject)
  private
    //SWbemLocator1: TSWbemLocator;
    fEventHandlers: TList;
    processesList: TList;
  protected
    fProcesses: TList;
    constructor Create;
//    procedure NotifyAll;

  public
    destructor Destroy; override;
    function GetLockedProcess(Path: String) :TList;
  end;

{==============================================================================}
implementation

constructor TProcessManager.Create;
begin
  inherited Create;
end;

destructor TProcessManager.Destroy;
begin
  inherited Destroy;
end;

function TProcessManager.GetLockedProcess(Path: String) :TList;

  function GetInfoTable(ATableType: DWORD): Pointer;
  var
    dwSize: DWORD;
    pPtr: Pointer;
    ntStatus: NT_STATUS;
  begin
    Result := nil;
    dwSize := WORD(-1);
    GetMem(pPtr, dwSize);
    ntStatus := ZwQuerySystemInformation(ATableType, pPtr, dwSize, nil);
    while ntStatus = STATUS_INFO_LENGTH_MISMATCH do
    begin
      dwSize := dwSize * 2;
      ReallocMem(pPtr, dwSize);
      ntStatus := ZwQuerySystemInformation(ATableType, pPtr, dwSize, nil);
    end;
    if ntStatus = STATUS_SUCCESS then
      Result := pPtr
    else
      FreeMem(pPtr);
  end;

  function GetFileNameThread(lpParameters: Pointer): DWORD; stdcall;
  var
    FileNameInfo: FILE_NAME_INFORMATION;
    ObjectNameInfo: TOBJECT_NAME_INFORMATION;
    IoStatusBlock: IO_STATUS_BLOCK;
    pThreadParam: TGetFileNameThreadParam;
    dwReturn: DWORD;
  begin
    ZeroMemory(@FileNameInfo, SizeOf(FILE_NAME_INFORMATION));
    pThreadParam := PGetFileNameThreadParam(lpParameters)^;
    Result := NtQueryInformationFile(pThreadParam.hFile, @IoStatusBlock,
      @FileNameInfo, MAX_PATH * 2, FileNameInformation);
    if Result = STATUS_SUCCESS then
    begin
      Result := NtQueryObject(pThreadParam.hFile, ObjectNameInformation,
        @ObjectNameInfo, MAX_PATH * 2, @dwReturn);
      if Result = STATUS_SUCCESS then
      begin
        pThreadParam.Status := Result;
        WideCharToMultiByte(CP_ACP, 0,
          @ObjectNameInfo.Name.Buffer[ObjectNameInfo.Name.MaximumLength -
          ObjectNameInfo.Name.Length],
          ObjectNameInfo.Name.Length, @pThreadParam.Data[0],
          MAX_PATH, nil, nil);
      end
      else
      begin
        pThreadParam.Status := STATUS_SUCCESS;
        Result := STATUS_SUCCESS;
        WideCharToMultiByte(CP_ACP, 0,
          @FileNameInfo.FileName[0], IoStatusBlock.Information,
          @pThreadParam.Data[0],
          MAX_PATH, nil, nil);
      end;
    end;
    PGetFileNameThreadParam(lpParameters)^ := pThreadParam;
    ExitThread(Result);
  end;

  function GetFileNameFromHandle(hFile: THandle): String;
  var
    lpExitCode: DWORD;
    pThreadParam: TGetFileNameThreadParam;
    hThread: THandle;
  begin
    Result := '';
    ZeroMemory(@pThreadParam, SizeOf(TGetFileNameThreadParam));
    pThreadParam.hFile := hFile;
    hThread := CreateThread(nil, 0, @GetFileNameThread, @pThreadParam, 0, PDWORD(nil)^);
    if hThread <> 0 then
    try
      case WaitForSingleObject(hThread, 100) of
        WAIT_OBJECT_0:
        begin
          GetExitCodeThread(hThread, lpExitCode);
          if lpExitCode = STATUS_SUCCESS then
            Result := pThreadParam.Data;
        end;
        WAIT_TIMEOUT:
          TerminateThread(hThread, 0);
      end;
    finally
      CloseHandle(hThread);
    end;
  end;

  function SetDebugPriv: Boolean;
  var
    Token: THandle;
    tkp: TTokenPrivileges;
  begin
    Result := false;
    if OpenProcessToken(GetCurrentProcess,
      TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token) then
    begin
      if LookupPrivilegeValue(nil, PChar('SeDebugPrivilege'),
        tkp.Privileges[0].Luid) then
      begin
        tkp.PrivilegeCount := 1;
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        Result := AdjustTokenPrivileges(Token, False,
          tkp, 0, PTokenPrivileges(nil)^, PDWord(nil)^);
      end;
    end;
  end;

type
  DriveQueryData = record
    DiskLabel: String;
    DiskDosQuery: String;
    DosQueryLen: Integer;
  end;

var
  hFile, hProcess: THandle;
  pHandleInfo: PSYSTEM_HANDLE_INFORMATION_EX;
  I, Drive: Integer;
  ObjectTypeNumber: Byte;
  FileDirectory, FilePath, ProcessName: String;
  SystemInformation, TempSI: PSYSTEM_PROCESS_INFORMATION;
  DosDevices: array [0..25] of DriveQueryData;
  LongFileName, TmpFileName: String;
  ProcHandles: array of integer;
  Tmp_proc: TProcess;
begin
  SetLength(LongFileName, MAX_PATH);
  GetLongPathNameA(PChar(Path), @LongFileName[1], MAX_PATH);

  for Drive := 0 to 25 do
  begin
    DosDevices[Drive].DiskLabel := Chr(Drive + Ord('a')) + ':';
    SetLength(DosDevices[Drive].DiskDosQuery, MAXCHAR);
    ZeroMemory(@DosDevices[Drive].DiskDosQuery[1], MAXCHAR);
    QueryDosDevice(PChar(DosDevices[Drive].DiskLabel),
      @DosDevices[Drive].DiskDosQuery[1], MAXCHAR);
    DosDevices[Drive].DosQueryLen := Length(PChar(DosDevices[Drive].DiskDosQuery));
    SetLength(DosDevices[Drive].DiskDosQuery, DosDevices[Drive].DosQueryLen);
  end;

  ObjectTypeNumber := 0;
  SetDebugPriv;
  hFile := CreateFile('NUL', GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
  if hFile = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    pHandleInfo := GetInfoTable(SystemHandleInformation);
    if pHandleInfo = nil then RaiseLastOSError;
    try
      for I := 0 to pHandleInfo^.NumberOfHandles - 1 do
        if pHandleInfo^.Information[I].Handle = hFile then
          if pHandleInfo^.Information[I].ProcessId = {FileId}GetCurrentProcessId then
          begin
            ObjectTypeNumber := pHandleInfo^.Information[I].ObjectTypeNumber;
            Break;
          end;
    finally
      FreeMem(pHandleInfo);
    end;
  finally
    CloseHandle(hFile);
  end;

  SystemInformation := GetInfoTable(SystemProcessesAndThreadsInformation);
  if SystemInformation <> nil then
  try
    pHandleInfo := GetInfoTable(SystemHandleInformation);
    if pHandleInfo <> nil then
    try
      //ProgressBar1.Position := 0;
      //ProgressBar1.Max := pHandleInfo^.NumberOfHandles;
      //Tmp_proc.Create;
      for I := 0 to pHandleInfo^.NumberOfHandles - 1 do
      begin
        if Tmp_proc = nil then
          Tmp_proc.Create;
        if pHandleInfo^.Information[I].ObjectTypeNumber = ObjectTypeNumber then
        begin
          hProcess := OpenProcess(PROCESS_DUP_HANDLE, True,
            pHandleInfo^.Information[I].ProcessId);
          if hProcess > 0 then
          try
            if DuplicateHandle(hProcess, pHandleInfo^.Information[I].Handle,
              GetCurrentProcess, @hFile, 0, False, DUPLICATE_SAME_ACCESS) then
            try
              if Application.Terminated then Exit;

              FilePath := GetFileNameFromHandle(hFile);
              if ((FilePath <> ''))then
              begin
                FileDirectory := '';
                for Drive := 0 to 25 do
                  if DosDevices[Drive].DosQueryLen > 0 then
                    if Copy(FilePath, 1, DosDevices[Drive].DosQueryLen) =
                      DosDevices[Drive].DiskDosQuery then
                    begin
                      FileDirectory := DosDevices[Drive].DiskLabel;
                      Delete(FilePath, 1, DosDevices[Drive].DosQueryLen);
                      Break;
                    end;

                if FileDirectory = '' then Continue;

                TempSI := SystemInformation;
                SetLength(TmpFileName, MAX_PATH);
                GetLongPathNameA(PChar(FileDirectory + FilePath), @TmpFileName[1], MAX_PATH);
                if (Pos(Path, TmpFileName)<>0) then
                begin
                  repeat
                    if TempSI^.ProcessID = pHandleInfo^.Information[I].ProcessId then
                        if  (Tmp_proc.Handle = TempSI^.ProcessID) then
                          begin
                            Tmp_proc.Files.Add(@TmpFileName);
                            Break;
                          end
                        else begin
                          processesList.Add(@Tmp_proc);
                          Tmp_proc.Destroy;
                          Break;
                        end;
                  TempSI := Pointer(DWORD(TempSI) + TempSI^.NextOffset);
                  until TempSI^.NextOffset = 0;
                  //Memo1.Lines.Add(ProcessName);
                end;
              end;
            finally
              CloseHandle(hFile);
            end;
          finally
            CloseHandle(hProcess);
          end;
        end;
        //ProgressBar1.Position := ProgressBar1.Position + 1;
        Application.ProcessMessages;
      end;
    finally
      FreeMem(pHandleInfo);
    end;
  finally
    FreeMem(SystemInformation);
  end;
end;

end.
 