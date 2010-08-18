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
  Classes, SysUtils, Windows, Communication, Device;

type
    {
        Description:
            Implements methods needed for IPC
    }
    TPipeConnector = class(TObject)
    private
        hPipeHandle: HANDLE; //pipe handle
        hThreadHandle: HANDLE; //thread handle
        fDevices: TList; //all the devices

        procedure Clear; //clears all the fields
        function GetDevice(index: integer): TDevice; //indexer function
        function GetDeviceCount: integer; //device count
        procedure GetDevicesInformation; //gets the devices information
        procedure PushDevice(device: TDevice;
            deviceInfo: DEVINFO); //pushes the device to the list
    public
        constructor Create; //constructor
        destructor Destroy; override; //destructor

        procedure EjectDevice(index: DEVINDEX); //tries to eject the device
        procedure Refresh; //causes a device list refresh
        property DeviceCount: integer read GetDeviceCount; //device count
        property Devices[index: integer]: TDevice read GetDevice; //devices
    end;


implementation

var
    csThreading: CRITICAL_SECTION; //for synchronization
    pself: TPipeConnector; //pointer to self

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
end;

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
        Clears all internal fields
    Parameters:
        None
    Return value:
        None
}
procedure TPipeConnector.Clear;
var
    i: integer; //loop index
begin
    //cleaning all devices
    for i := 0 to fDevices.Count-1 do
    begin
        TDevice(fDevices.Items[i]).Destroy;
    end;
    //clearing list
    fDevices.Clear;
end; //Clear

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
    numwr: DWORD; //number of written bytes
    numrd: DWORD; //number of read bytes
    info: OPINFO; //operation info
begin
    //sending an ejection request
    info.dwOpCode := DEVICE_EJECT_REQUEST;
    info.dwOpStatus := OPERATION_START;
    WriteFile(self.hPipeHandle, info, sizeof(OPINFO), numwr, nil);
    WriteFile(self.hPipeHandle, index, sizeof(INDEX), numwr, nil);
    //getting the answer
    ReadFile(self.hPipeHandle, info, sizeof(OPINFO), numrd, nil);
    //if the operation was successful, we return from the function
    //otherwise, we start to handle the progress callbacks
    if (info.dwOpCode <> DEVICE_EJECT_ACCEPT)
    then begin
        //TODO: implement callbacks
    end;
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
    info: OPINFO; //operation info cache
    numrd: DWORD; //number ow bytes read
    deviceInfo: DEVINFO; //device info cache
    i: Integer; //loop index
    strbuf: LPWSTR; //mount points buffer
    device: TDevice; //temporary buffer for device
begin
    //clearing device buffers
    self.Clear;
    //reading service information
    //ReadFile(self.hPipeHandle, info, sizeof(OPINFO), numrd, nil);
    //if ((info.dwOpCode = DEVICE_REFRESH_ANSWER) and
    //    (info.dwOpStatus = OPERATION_START))
//    then begin
        //starting to read device information
        ReadFile(self.hPipeHandle, info, sizeof(OPINFO), numrd, nil);
        //reading until the OPERATION_FINISH is received
        while (info.dwOpStatus = OPERATION_PROGRESS) do
        begin
            //reading device information
            ReadFile(self.hPipeHandle, deviceInfo, sizeof(DEVINFO), numrd, nil);
            //creating a new device
            device := TDevice.Create(deviceInfo);
            //reading device mount points (if there are any)
            for i := 0 to deviceInfo.dwMountPtsCount-1 do
            begin
                ReadFile(self.hPipeHandle, strbuf, MAX_PATH, numrd, nil);
                device.AddMountPoint(strbuf);
            end;
            //pushing device to the list
            PushDevice(device, deviceInfo);
            //reading next portion of information from the pipe
            ReadFile(self.hPipeHandle, info, sizeof(OPINFO), numrd, nil);
        end;
//    end;
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
   info: OPINFO; //operation info
   numwr: DWORD; //numer of written bytes
begin
    try
        //suspending listener thread in order not to get refresh
        //informatin twice
        //SuspendThread(self.hThreadHandle);
        ////entering critical section
        //EnterCriticalSection(csThreading);
        info.dwOpCode := DEVICE_REFRESH_REQUEST;
        info.dwOpStatus := OPERATION_START;
        //sending a refreash request. If it was sent successfully,
        //we get the devices information
        {if} WriteFile(self.hPipeHandle, info, sizeof(OPINFO), numwr, nil);
        //then begin
        //    self.GetDevicesInformation;
        //end;
    finally
        ////leaving critical section
        //LeaveCriticalSection(csThreading);
        ////resuming listener thread
        //ResumeThread(self.hThreadHandle);
    end;
end; //Refresh

{
    Purpose:
        Thread function which monitors the pipe
}
function MonitorFunction(parameters: Pointer): DWORD; stdcall;
var
    info: OPINFO; //current operation information
    numrd: DWORD; //number of bytes read
    hPipe: HANDLE; //pointer to self
begin
    //typecasting
    hPipe := HANDLE(parameters^);
    //reading file information
    while ReadFile(hPipe, info, sizeof(OPINFO), numrd, nil) do
    begin
        case info.dwOpCode of
            DEVICE_REFRESH_ANSWER:
            begin
                try
                    EnterCriticalSection(csThreading);
                    pself.GetDevicesInformation;
                finally
                    LeaveCriticalSection(csThreading);
                end;
            end; //DEVICE_REFRESH_ANSWER
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
     inherited Create;
     //connecting to the named pipe
     WaitNamedPipeW(@PIPE_NAME[1], DWORD(NMPWAIT_WAIT_FOREVER));
     self.hPipeHandle := CreateFileW(@PIPE_NAME[1], GENERIC_READ or GENERIC_WRITE,
           FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
           FILE_ATTRIBUTE_NORMAL, 0);
     //if the connetion failed - throw an exception
     if (self.hPipeHandle = INVALID_HANDLE_VALUE)
     then begin
          //TODO: add exception support!
     end;
     //saving self pointer
     pself := self;
     //creating device list
     fDevices := TList.Create;
     //initializing critical section
     InitializeCriticalSection(csThreading);
     //creating monitor thread
     self.hThreadHandle := CreateThread(nil, 0, @MonitorFunction,
        LPCVOID(@self.hPipeHandle), 0, threadId);
end; //constructor

{
    Purpose:
        Destructor
}
destructor TPipeConnector.Destroy;
begin
    //terminating the monitor thread
    TerminateThread(self.hThreadHandle, NO_ERROR);
    //deleting critical section
    DeleteCriticalSection(csThreading);
    //closing the pipe
    CloseHandle(self.hPipeHandle);
    inherited Destroy;
end; //destructor

end.

