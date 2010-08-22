{
    Connector.pas
    Started: 16.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Pipe connector declaration and implementation.
    This class is for interprocess communication
}
unit Connector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Communication, Device, Process;

type
    {
        Description:
            Implements methods needed for IPC
    }
  TPipeConnector = class(TObject)
  private
    hInPipeHandle:   HANDLE; //in pipe handle
    hOutPipeHandle: HANDLE; //out pipe handle
    hThreadHandle: HANDLE; //thread handle
    fDevices:      TList;  //all the devices
    fProcesses: TList; //all the processes
    fRemovalFailed: TNotifyEvent; //device removal failed
    fRemovalSucceeded: TNotifyEvent; //device removal succeeded
    fSearchProgress: TNotifyEvent; //progress notification event
    fRefreshFinished: TNotifyEvent; //refresh finished event

    procedure ClearDevices; //clears devices
    procedure ClearProcesses; //clears processes
    function GetDevice(index: integer): TDevice; //indexer function
    function GetDeviceCount: integer; //device count
    procedure GetBlockersInfo; //gets the blocking processes info
    procedure GetDevicesInformation; //gets the devices information
    procedure GetLockerList; //gets the locking processes list
    function GetProcess(index: integer): TProcess; //indexer function
    function GetProcessCount: integer; //property function
    procedure GetProgressInformation; //gets the progress information
    procedure PushDevice(device: TDevice; deviceInfo: DEVINFO); //adds device

  public

    constructor Create; //constructor
    destructor Destroy; override; //destructor

    property DeviceCount: integer read GetDeviceCount; //device count
    property Devices[index: integer]: TDevice read GetDevice; //devices
    procedure EjectDevice(index: DEVINDEX); //tries to eject the device
    property ProcessCount: integer read GetProcessCount; //process count
    property Processes[index: integer]: TProcess read GetProcess; //processes
    procedure Refresh; //causes a device list refresh

    //events
    property OnRefreshFinished: TNotifyEvent
        read fRefreshFinished write fRefreshFinished;
    property OnRemovalFailed: TNotifyEvent
        read fRemovalFailed write fRemovalFailed;
    property OnRemovalSucceeded: TNotifyEvent
        read fRemovalSucceeded write fRemovalSucceeded;
    property OnSearchProgress: TNotifyEvent
        read fSearchProgress write fSearchProgress;
  end;


implementation

var
  csThreading: CRITICAL_SECTION; //for synchronization
  pself: TPipeConnector; //pointer to self

{
    Purpose:
        Gets the total blocking processes count
    Parameters:
        None
    Return value:
        Total number of blocking processes
}
function TPipeConnector.GetProcessCount: integer;
begin
    try
        EnterCriticalSection(csThreading);
        Result := Self.fProcesses.Count;
    finally
        LeaveCriticalSection(csThreading);
    end;
end; //GetProcessCount

{
    Purpose:
        Gets the process by its index
    Parameters:
        index - index of the process
    Return value:
        A pointer to the process' instance
}
function TPipeConnector.GetProcess(index: integer): TProcess;
begin
    try
        //synchronizing
        EnterCriticalSection(csThreading);
        Result := TProcess(Self.fProcesses.Items[index]);
    finally
        LeaveCriticalSection(csThreading);
    end;
end; //GetProcess

{
    Purpose:
        Gets the locking processes and the files opened by them
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.GetLockerList;
var
    info: OPINFO; //operation info
    numrd: DWORD; //number of read bytes
    procInfo: PROC_INFO; //process info
    i: integer; //loop index
    process: TProcess; //process pointer
    stringbuf: TWideCharArray; //wide string buffer
begin
    //reading service information twice - START and first PROGRESS
    ReadFile(Self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
    while ((info.dwOpCode = PROCESS_LOCK_INFO) and
        (info.dwOpStatus = OPERATION_PROGRESS)) do
    begin
        //reading process information
        ReadFile(Self.hInPipeHandle, procInfo, sizeof(PROC_INFO), numrd, nil);
        process := TProcess.Create(procInfo);
        //reading blocked files path
        for i := 0 to procInfo.LockedFilesCount-1 do
        begin
            ReadFile(Self.hInPipeHandle, stringbuf, MAX_PATH, numrd, nil);
            process.AddFileName(stringbuf);
        end;
        //adding new process to the list
        Self.fProcesses.Add(process);
        //reading next portion of information
        ReadFile(Self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
    end;
end; //GetLockerList

{
    Purpose:
        Gets the progress information (in percent)
        and calls the event handler
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.GetProgressInformation;
var
  numrd: DWORD;  //number of read bytes
  info:  OPINFO; //operation info
  percentage: UCHAR; //progress percentage
begin
    //reading next portion of service info from the pipe
    ReadFile(Self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
    while ((info.dwOpCode = PROCESS_SEARCH_HANDLES) and
        (info.dwOpStatus = OPERATION_PROGRESS)) do
    begin
        //reading the execution percentage
        ReadFile(Self.hInPipeHandle, percentage, sizeof(UCHAR), numrd, nil);
        if Assigned(Self.fSearchProgress)
        then begin
            //notifying the listener
            Self.fSearchProgress(TObject(@percentage));
        end;
        //reading next operation information
        ReadFile(Self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
    end;
end; //GetProgressInformation

{
    Purpose:
        Wraps the functionality needed for handling of the
        "Device eject failed" event
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.GetBlockersInfo;
var
  numrd: DWORD;  //number of read bytes
  info:  OPINFO; //operation info buffer
begin
  //reading dext service information
  ReadFile(Self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
  //checking the condition - if the search process has started
  if ((info.dwOpCode = PROCESS_SEARCH_HANDLES) and
    (info.dwOpStatus = OPERATION_START)) then
  begin
    //if the operation is in the progress, we get the progress
    //info and send it to the listeners
    Self.GetProgressInformation;
    //synchronization is done for getter
    try
        EnterCriticalSection(csThreading);
        //getting locker processes
        Self.GetLockerList;
    finally
        LeaveCriticalSection(csThreading);
    end;
    //reading finishing service information
    ReadFile(Self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
  end;
end; //GetBlockersInfo

{
    Purpose:
        Gets the total device count
    Parameters:
        None
    Return value:
        Count of top-level devices
}
function TPipeConnector.GetDeviceCount: integer;
begin
  try
    EnterCriticalSection(csThreading);
    Result := fDevices.Count;
  finally
    LeaveCriticalSection(csThreading);
  end;
end; //GetDeviceCount

{
    Purpose:
        Adds a new device to the device list
    Parameters:
        device - pointer to the device
        deviceInfo - DEVINFO structure
    Return value:
        None
}
procedure TPipeConnector.PushDevice(device: TDevice; deviceInfo: DEVINFO);
var
  pdev: TDevice; //pointer to the device
begin
  pdev := nil;
  case deviceInfo.devIndex.dwDeviceLevel of
    1: //device is a USB dev
    begin
      fDevices.Insert(deviceInfo.devIndex.dwDeviceNumber, device);
    end;
    2: //device is a drive
    begin
      pdev := TDevice(fDevices.Items[deviceInfo.devIndex.dwTopIndex]);
      pdev.AddChild(device);
    end;
    3: //device is a volume
    begin
      pdev := TDevice(fDevices.Items[deviceInfo.devIndex.dwTopIndex]);
      pdev.Children[deviceInfo.dwParent].AddChild(device);
    end;
  end;
end; //PushDevice

{
    Purpose:
        Gets the device specified by device index
    Input parameters:
        index - device index in the list
    Return value:
        A device pointer
}
function TPipeConnector.GetDevice(index: integer): TDevice;
begin
  try
    EnterCriticalSection(csThreading);
    Result := TDevice(fDevices.Items[index]);
  finally
    LeaveCriticalSection(csThreading);
  end;
end; //GetDevice

{
    Purpose:
        Clears devices list
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.ClearDevices;
var
  i: integer; //loop index
begin
  //cleaning all devices
  for i := 0 to fDevices.Count - 1 do
  begin
    TDevice(fDevices.Items[i]).Destroy;
  end;
  //clearing list
  fDevices.Clear;
end; //ClearDevices

{
    Purpose:
        Clears processes list
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.ClearProcesses;
var
  i: integer; //loop index
begin
  //cleaning all devices
  for i := 0 to fProcesses.Count - 1 do
  begin
    TProcess(fProcesses.Items[i]).Destroy;
  end;
  //clearing list
  fProcesses.Clear;
end; //ClearProcesses


{
    Purpose:
        Tries to eject the device. If the removal failed,
        starts to send callbacks to listeners
    Parameters:
        index - device index (from the received data)
    Return value:
        None
}
procedure TPipeConnector.EjectDevice(index: DEVINDEX);
var
  numwr: DWORD;  //number of written bytes
  info:  OPINFO; //operation info
begin
  //sending an ejection request
  info.dwOpCode   := DEVICE_EJECT_REQUEST;
  info.dwOpStatus := OPERATION_START;
  WriteFile(self.hOutPipeHandle, info, sizeof(OPINFO), numwr, nil);
  WriteFile(self.hOutPipeHandle, index, sizeof(INDEX), numwr, nil);
end; //EjectDevice

{
    Purpose:
        Gets the devices information after the signal
        DEVICE_REFRESH_ANSWER received
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.GetDevicesInformation;
var
  info:   OPINFO;  //operation info cache
  numrd:  DWORD;   //number ow bytes read
  deviceInfo: DEVINFO; //device info cache
  i:      integer; //loop index
  strbuf: TWideCharArray;  //mount points buffer
  device: TDevice; //temporary buffer for device
begin
  //clearing device buffers
  self.ClearDevices;
  //starting to read device information
  ReadFile(self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
  //reading until the OPERATION_FINISH is received
  while (info.dwOpStatus = OPERATION_PROGRESS) do
  begin
    //reading device information
    ReadFile(self.hInPipeHandle, deviceInfo, sizeof(DEVINFO), numrd, nil);
    //creating a new device
    device := TDevice.Create(deviceInfo);
    //reading device mount points (if there are any)
    for i := 0 to deviceInfo.MountPtsCount - 1 do
    begin
      ReadFile(self.hInPipeHandle, strbuf, MAX_PATH, numrd, nil);
      device.AddMountPoint(strbuf);
    end;
    //pushing device to the list
    PushDevice(device, deviceInfo);
    //reading next portion of information from the pipe
    ReadFile(self.hInPipeHandle, info, sizeof(OPINFO), numrd, nil);
  end;
end; //GetDevicesInformation

{
    Purpose:
        Causes a device list refresh
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.Refresh;
var
  info:  OPINFO; //operation info
  numwr: DWORD;  //numer of written bytes
begin
  //setting query parameters
  info.dwOpCode   := DEVICE_REFRESH_REQUEST;
  info.dwOpStatus := OPERATION_START;
  //sending a refreash request. If it was sent successfully,
  //we get the devices information (in the monitor thread)
  WriteFile(self.hOutPipeHandle, info, sizeof(OPINFO), numwr, nil);
end; //Refresh

{
    Purpose:
        Thread function which monitors the pipe
}
function MonitorFunction(parameters: Pointer): DWORD; stdcall;
var
  info:  OPINFO; //current operation information
  numrd: DWORD;  //number of bytes read
begin
  //reading file information
  while ReadFile(pself.hInPipeHandle, info, sizeof(OPINFO), numrd, nil) do
  begin
    //choosing the operation, depending on received code
    case info.dwOpCode of
      //device refresh information
      DEVICE_REFRESH_ANSWER:
      begin
        //starting to get information
        if (info.dwOpStatus = OPERATION_START) then
        begin
          try
            EnterCriticalSection(csThreading);
            //getting devices information
            pself.GetDevicesInformation;
            //firing the finish event
            if Assigned(pself.fRefreshFinished)
            then begin
                pself.fRefreshFinished(nil);
            end;
          finally
            LeaveCriticalSection(csThreading);
          end;
        end;
      end; //DEVICE_REFRESH_ANSWER
      //device was ejected successfully - we do a notification
      DEVICE_EJECT_ACCEPT:
      begin
        //signaling about the successful removal
        if Assigned(pself.fRemovalSucceeded)
        then begin
            pself.fRemovalSucceeded(nil);
        end;
      end; //DEVICE_EJECT_ACCEPT
      //ejection failed
      DEVICE_EJECT_REJECT:
      begin
        //signaling about the failure
        if Assigned(pself.fRemovalFailed)
        then begin
            pself.fRemovalFailed(nil);
        end;
        //getting blockers info
        pself.GetBlockersInfo;
      end; //DEVICE_EJECT_REJECT
    end;
  end;
  //we should never be here...
  Result := NO_ERROR;
end;

{
    Purpose:
        Constructor. Initializes a pipe connector
    Parameters:
        None
}
constructor TPipeConnector.Create;
var
  threadId: DWORD;
begin
    //TODO: implement exceptions on timeout and do application
    //termination support in case there's no connection to the service
    inherited Create;
    //connecting to the named pipe servier->client
    WaitNamedPipeW(@PIPE_INCOMING_NAME[1], DWORD(NMPWAIT_WAIT_FOREVER));
    Self.hInPipeHandle := CreateFileW(@PIPE_INCOMING_NAME[1], GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    //creating second communication channel client->server
    Self.hOutPipeHandle := CreateNamedPipeW(@PIPE_OUTGOING_NAME[1],
          PIPE_ACCESS_OUTBOUND, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT,
          1, 0, 0, 200, nil);
    if ((Self.hInPipeHandle = INVALID_HANDLE_VALUE) or
      (Self.hOutPipeHandle = INVALID_HANDLE_VALUE)) then
    begin
      //TODO: add exception support!
    end;
    if (not ConnectNamedPipe(Self.hOutPipeHandle, nil))
    then begin
      //TODO: exceptions
    end;
    //initializing events with the null values
    Self.fRemovalSucceeded := nil;
    Self.fRemovalFailed := nil;
    Self.fSearchProgress := nil;
    Self.fRefreshFinished := nil;
    //saving self pointer (for threading support)
    pself    := self;
    //creating device list
    fDevices := TList.Create;
    //creating process list
    fProcesses := TList.Create;
    //initializing critical section
    InitializeCriticalSection(csThreading);
    //creating monitor thread
    self.hThreadHandle := CreateThread(nil, 0, @MonitorFunction,
      LPCVOID(nil), 0, threadId);
end; //constructor

{
    Purpose:
        Destructor
}
destructor TPipeConnector.Destroy;
begin
    //clearing internal data
    self.ClearDevices;
    self.fDevices.Destroy;
    self.ClearProcesses;
    self.fProcesses.Destroy;
    //terminating the monitor thread
    try
        EnterCriticalSection(csThreading);
        TerminateThread(self.hThreadHandle, NO_ERROR);
    finally
        LeaveCriticalSection(csThreading);
    end;
    //deleting critical section
    DeleteCriticalSection(csThreading);
    //closing the pipes
    DisconnectNamedPipe(Self.hOutPipeHandle);
    CloseHandle(self.hInPipeHandle);
    CloseHandle(self.hOutPipeHandle);
    inherited Destroy;
end; //destructor

end.

