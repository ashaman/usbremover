unit NTdll;

interface

uses
  Windows, SysUtils;

  type
  NT_STATUS = Cardinal;

  TFileDirectoryInformation = packed record
    NextEntryOffset: ULONG;
    FileIndex: ULONG;
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    FileAttributes: ULONG;
    FileNameLength: ULONG;
    FileName: array[0..0] of WideChar;
  end;
  FILE_DIRECTORY_INFORMATION = TFileDirectoryInformation;
  PFileDirectoryInformation = ^TFileDirectoryInformation;
  PFILE_DIRECTORY_INFORMATION = PFileDirectoryInformation;

  PSYSTEM_THREADS = ^SYSTEM_THREADS;
  SYSTEM_THREADS  = packed record
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    WaitTime: ULONG;
    StartAddress: Pointer;
    UniqueProcess: DWORD;
    UniqueThread: DWORD;
    Priority: Integer;
    BasePriority: Integer;
    ContextSwitchCount: ULONG;
    State: Longint;
    WaitReason: Longint;
  end;

  PSYSTEM_PROCESS_INFORMATION = ^SYSTEM_PROCESS_INFORMATION;
  SYSTEM_PROCESS_INFORMATION = packed record
    NextOffset: ULONG;
    ThreadCount: ULONG;
    //Reserved1: array [0..5] of ULONG; // Что такое, пока не понятно...
    CreateTime: FILETIME;
    UserTime: FILETIME;
    KernelTime: FILETIME;
    ModuleNameLength: WORD;
    ModuleNameMaxLength: WORD;
    ModuleName: PWideChar;
    BasePriority: ULONG;
    ProcessID: ULONG;
    InheritedFromUniqueProcessID: ULONG;
    HandleCount: ULONG;
    //Reserved2 : array[0..1] of ULONG; // Что такое, пока не понятно...
    PeakVirtualSize : ULONG;
    VirtualSize : ULONG;
    PageFaultCount : ULONG;
    PeakWorkingSetSize : ULONG;
    WorkingSetSize : ULONG;
    QuotaPeakPagedPoolUsage : ULONG;
    QuotaPagedPoolUsage : ULONG;
    QuotaPeakNonPagedPoolUsage : ULONG;
    QuotaNonPagedPoolUsage : ULONG;
    PageFileUsage : ULONG;
    PeakPageFileUsage : ULONG;
    PrivatePageCount : ULONG;
    ReadOperationCount : LARGE_INTEGER;
    WriteOperationCount : LARGE_INTEGER;
    OtherOperationCount : LARGE_INTEGER;
    ReadTransferCount : LARGE_INTEGER;
    WriteTransferCount : LARGE_INTEGER;
    OtherTransferCount : LARGE_INTEGER;
    ThreadInfo: array [0..0] of SYSTEM_THREADS;
  end;

  PSYSTEM_HANDLE_INFORMATION = ^SYSTEM_HANDLE_INFORMATION;
  SYSTEM_HANDLE_INFORMATION = packed record
    ProcessId: DWORD;
    ObjectTypeNumber: Byte;
    Flags: Byte;
    Handle: Word;
    pObject: Pointer;
    GrantedAccess: DWORD;
  end;

  PSYSTEM_HANDLE_INFORMATION_EX = ^SYSTEM_HANDLE_INFORMATION_EX;
  SYSTEM_HANDLE_INFORMATION_EX = packed record
    NumberOfHandles: dword;
    Information: array [0..0] of SYSTEM_HANDLE_INFORMATION;
  end;

  PFILE_NAME_INFORMATION = ^FILE_NAME_INFORMATION;
  FILE_NAME_INFORMATION = packed record
    FileNameLength: ULONG;
    FileName: array [0..MAX_PATH - 1] of WideChar;
  end;

  PUNICODE_STRING = ^TUNICODE_STRING;
  TUNICODE_STRING = packed record
    Length : WORD;
    MaximumLength : WORD;
    Buffer : array [0..MAX_PATH - 1] of WideChar;
  end;

  POBJECT_NAME_INFORMATION = ^TOBJECT_NAME_INFORMATION;
  TOBJECT_NAME_INFORMATION = packed record
    Name : TUNICODE_STRING;
  end;

  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  IO_STATUS_BLOCK = packed record
    Status: NT_STATUS;
    Information: DWORD;
  end;

  PGetFileNameThreadParam = ^TGetFileNameThreadParam;
  TGetFileNameThreadParam = packed record
    hFile: THandle;
    Data: array [0..MAX_PATH - 1] of Char;
    Status: NT_STATUS;
  end;

const
  STATUS_SUCCESS = NT_STATUS($00000000);
  STATUS_INVALID_INFO_CLASS = NT_STATUS($C0000003);
  STATUS_INFO_LENGTH_MISMATCH = NT_STATUS($C0000004);
  STATUS_INVALID_DEVICE_REQUEST = NT_STATUS($C0000010);
  ObjectNameInformation = 1;
  FileDirectoryInformation = 1;
  FileNameInformation = 9;
  SystemProcessesAndThreadsInformation = 5;
  SystemHandleInformation = 16;

  
  function ZwQuerySystemInformation(ASystemInformationClass: DWORD;
    ASystemInformation: Pointer; ASystemInformationLength: DWORD;
    AReturnLength: PDWORD): NT_STATUS; stdcall; external 'ntdll.dll';

  function NtQueryInformationFile(FileHandle: THandle;
    IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: Pointer;
    Length: DWORD; FileInformationClass: DWORD): NT_STATUS;
    stdcall; external 'ntdll.dll';

  function NtQueryObject(ObjectHandle: THandle;
    ObjectInformationClass: DWORD; ObjectInformation: Pointer;
    ObjectInformationLength: ULONG;
    ReturnLength: PDWORD): NT_STATUS; stdcall; external 'ntdll.dll';

  function GetLongPathNameA(lpszShortPath, lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall; external kernel32;

implementation

end.
 