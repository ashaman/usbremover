unit DeviceManager;

interface
uses
  Windows, Classes, Device, DeviceException;

type
  //Abstract class for device manager
  TDeviceManager = class(TObject)
  private
    fDeviceType: UINT;
    fEventHandlers: TList;
    procedure GetDrives;
  protected
    fDevices: TList;
    constructor Create(deviceType: integer);
    procedure NotifyAll;
    function FilterDevices(drivePath: PChar): boolean;
  public
    destructor Destroy; override;

    //These functions depend on device type
    procedure RemoveDrive; virtual; abstract;
    procedure ForcedRemoveDrive; virtual; abstract;

    //These funtions are common for all devices
    function GetBlockedFiles: TStrings;
    function GetBlockerID: HWND;
    function GetDeviceInfo(handle: THandle): TDevice; overload;
    function GetDeviceInfo(name: PChar): TDevice; overload;

    //These functions add event handlers from listeners
    procedure AddHandler(Handler: TNotifyEvent);
    procedure RemoveHandler(Handler: TNotifyEvent);
  end;

{==============================================================================}
implementation

uses SysUtils;

//Filters only necessary devices which match the type criteria
function TDeviceManager.FilterDevices(drivePath: PChar): boolean;
begin
  Result := false;
  if GetDriveType(drivePath) = fDeviceType
  then begin
    Result := true;
  end;
end;

//This function gets all logical drives in system
procedure TDeviceManager.GetDrives;
const
  charCount = 4; //four characters describe each drive, e.g.: c:\<null-term>
var
  drives: PAnsiChar; //buffer for driver strings
  pDrives: Pointer; //pointer to the drives array
  bufSize: DWORD; //size of buffer
  i: integer; //counter
  driveNumber: integer; //number of drives
  sizeOfChar: integer; {size of 1 character in bytes:
                       (ANSI - 1 byte, Unicode - 2 bytes)}
begin
  //here we get size needed for buffer
  bufSize := GetLogicalDriveStrings(0,nil);
  if bufSize<>0
  then begin {everything is OK}
    drives := AllocMem(bufSize); //we alloc memory
    pDrives := drives; //save pointer to the beginning odf the array
    GetLogicalDriveStrings(bufSize,drives);
    sizeOfChar := sizeof(drives[0]);
    driveNumber := (bufSize-1) div charCount; //we count the quantity of drives
    for i := 0 to driveNumber-1 do
    begin
      if FilterDevices(drives)
      then begin
        fDevices.Add(TDevice.Create(drives));
      end; {filter}
      drives := drives + charCount*sizeOfChar;  //move to the next list item
    end; {drives}
    FreeMem(pDrives,bufSize); //we release resources
  end {bufSize<>0}
  else begin
    raise EDeviceException.Create('Initialization failed!');
  end; {Raise}
end; {GetDrives}

//Constructor
constructor TDeviceManager.Create(deviceType: integer);
begin
  inherited Create;
  fEventHandlers := TList.Create;
  fDevices := TList.Create;
  self.fDeviceType := deviceType;
  GetDrives;
end;

//Destructor
destructor TDeviceManager.Destroy;
var
  i: integer;
begin
  for i := 0 to fEventHandlers.Count-1 do
  begin
    FreeMem(fEventHandlers.Items[i]);
  end;
  for i := 0 to fDevices.Count-1 do
  begin
    TDevice(fDevices.Items[i]).Destroy;
  end;
  fEventHandlers.Destroy;
  fDevices.Destroy;
  inherited Destroy;
end;

function TDeviceManager.GetBlockerID: HWND;
begin
  Result := 0;
end;

function TDeviceManager.GetBlockedFiles: TStrings;
begin
  Result := nil;
end;

//This function searches the device by its handle
//If such a device does not exist, this function throws EDeviceException
function TDeviceManager.GetDeviceInfo(handle: THandle): TDevice;
var
  i: integer;
  found: boolean;
begin
  i := 0;
  found := false;
  Result := nil;
  while (i<=self.fDevices.Count-1) and not found do
  begin
    if TDevice(fDevices.Items[i]).Handle = handle
    then begin
      Result := TDevice(fDevices.Items[i]);
      found := true;
    end {if-handle}
    else begin
      inc(i);
    end; {else}
  end; {while}
  if not found
  then begin
    raise EDeviceException.Create('Device with the specified handle does not exist!');
  end; {not found}
end; {GetDeviceInfo}

function TDeviceManager.GetDeviceInfo(name: PChar): TDevice;
begin
  Result := nil;
end;

//Adds new listener to list
procedure TDeviceManager.AddHandler(Handler: TNotifyEvent);
begin
  if (fEventHandlers.IndexOf(@Handler) = -1) //is not in the list
  then begin
    fEventHandlers.Add(@Handler);
  end;
end;

//Removes event listener
procedure TDeviceManager.RemoveHandler(Handler: TNotifyEvent);
begin
  fEventHandlers.Remove(@Handler);
end;

//Notifies all listeners about changes in this object
procedure TDeviceManager.NotifyAll;
var
  i, count: integer;
  event: TNotifyEvent;
begin
  count := fEventHandlers.Count;
  for i := 0 to count-1 do
  begin
    event := TNotifyEvent(fEventHandlers.Items[i]^);
    if Assigned(event)
    then begin
      event(Self);
    end;
  end;
end;

end.
