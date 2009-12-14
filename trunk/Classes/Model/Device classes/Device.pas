{
  Device class.
  Developed by J.L. Blackrow.
}

{
  TODO: Getting access rights for scanning system information
}

unit Device;

interface

uses
  WinIOCtl, Classes, WMI;

type
  //Aliases for data types
  TBusType = STORAGE_BUS_TYPE;
  TDeviceInfoData = SP_DEVINFO_DATA;

  //Kind of value token from device properties 
  TValueKind =
  (
    vkNumber,
    vkString
  );

  //Device class
  TDevice = class(TObject)
  private
    fCapabilities: {Cardinal;} TDeviceCapabilities; //device capabilities
    fClassGUID: TGUID; //class GUID
    fDescription: string; //device description string
    fDeviceClassName: string; //class name
    fManufacturer: string; //device manufacturer
    function GetChild(index: integer): TDevice;
    function GetCount: integer;
    procedure GetSetupAPIInforamtion(ClassGUID: TGUID);
    function GetDeviceInformation(DeviceNumber: TStorageDeviceNumber;
      ClassGUID: TGUID; DeviceInfoSet: THandle): TDeviceInfoData; overload;
    function GetDeviceInformation(InstanceHandle: THandle;
      DeviceInfoSet: THandle): TDeviceInfoData; overload;
    function GetDeviceInformationSet(ClassGUID: TGUID): THandle;
    function GetDeviceNumber(FileHandle: THandle): TStorageDeviceNumber;
    function GetInstanceHandle: THandle;
    function GetPropertyBufferSize(ValueKind: TValueKind): Cardinal;
  protected
    fBusType: TBusType; //device bus type
    fChildren: TList; //child devices
    fDeviceInfoData: TDeviceInfoData; //device info data
    fDeviceInfoSet: Cardinal; //device info set
    fDeviceNumber: TStorageDeviceNumber; //device number
    fFriendlyName: string; //device full name
    fParent: TDevice; //parent device of the object. If it is not removable - is null
    fPath: string; //device path (full)
    function GetBusType(Path: string): TBusType;
    function GetDeviceProperty(DeviceInformation: TDeviceInfoData;
      PropertyCode: integer; DeviceInfoSet: THandle; ValueKind: TValueKind): PByte;
    function GetMountPoints: TStringList; virtual; //ADDED LATELY!
    constructor Create(ClassGUID: TGUID; Path: string); overload;
    constructor Create(ClassGUID: TGUID; InstanceHandle: Cardinal); overload;
  public
    destructor Destroy; override;
    procedure NotifySystem; virtual;
    procedure AddChild(Device: TDevice);
    property BusType: TBusType read fBusType;
    property Capabilities: {Cardinal} TDeviceCapabilities read fCapabilities;
    property Children [index: integer]: TDevice read GetChild;
    property ClassGUID: TGUID read fClassGUID;
    property Count: integer read GetCount;
    property Description: string read fDescription;
    property DeviceClassName: string read fDeviceClassName;
    property FriendlyName: string read fFriendlyName;
    property InstanceHandle: THandle read GetInstanceHandle;
    property Manufacturer: string read fManufacturer;
    property MountPoints: TStringList read GetMountPoints;
    property Parent: TDevice read fParent;
    class function FormatDevicePath(const Path: string): string;
  end;

implementation

uses
  Windows, DeviceException, SysUtils;

//This function implements abstract member of TDevice class
function TDevice.GetMountPoints: TStringList;
var
  i: integer; //loop index
begin
  Result := TStringList.Create;
  for i := 0 to fChildren.Count-1 do
  begin
    Result.AddStrings(TDevice(fChildren.Items[i]).GetMountPoints);
  end;
end; //GetMountPoints

//This procedure adds a new device to children list
procedure TDevice.AddChild(Device: TDevice);
begin
  Device.fParent := self;
  fChildren.Add(Device);
end; //AddChild

//This function returns the total count of child devices
function TDevice.GetCount: integer;
begin
  Result := fChildren.Count;
end; //GetCount

//This function gets the child of this device specified by index
function TDevice.GetChild(index: integer): TDevice;
begin
  Result := TDevice(fChildren.Items[index]);
end; //GetChild

//This function gets device instance handle
function TDevice.GetInstanceHandle: THandle;
begin
  Result := fDeviceInfoData.DevInst;
end;

//This function notifies the OS about the device removal
procedure TDevice.NotifySystem;
var
  i: integer; //loop index
begin
  for i := 0 to fChildren.Count-1 do
  begin
    TDevice(fChildren.Items[i]).NotifySystem;
  end;
end;

//Formats the device path for opening it as file
class function TDevice.FormatDevicePath(const Path: string): string;
const
  dot = '.';
  question = '?'; //question mark
  slash = '\'; //backslash
var
  lastSlash: integer; //last backslash position
begin
  Result := Path;
  //we get last backslash index
  lastSlash := LastDelimiter(slash, Path);
  //if it is in the end, we delete it
  if lastSlash = Length(Path)
  then begin
    Delete(Result, lastSlash, 1);
  end;
  //we change question mark to dot
  if Result[3] = question
  then begin
    Result[3] := dot;
  end;
end; //FormatDevicePath

//This function gets device number using the DeviceIoControl function
function TDevice.GetDeviceNumber(FileHandle: THandle): TStorageDeviceNumber;
var
  dummy: Cardinal; //returned bytes
begin
  //getting device number through DeviceIoContrtol and TStorageDeviceNumber
  if not DeviceIoControl(FileHandle,IOCTL_STORAGE_GET_DEVICE_NUMBER, nil, 0,
    @Result, sizeof(Result), dummy, nil)
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then
end; //GetDeviceNumber

//Gets buffer and returns its size
function TDevice.GetPropertyBufferSize(ValueKind: TValueKind): Cardinal;
begin
  Result := 0;
  case ValueKind of
    vkNumber:
      begin
        Result := sizeof(Integer);
      end;
    vkString:
      begin
        Result := sizeof(TCharArray);
      end;
  end; //case
end; //GetPropertyBuffer

//This function returns the property specified by its control code
function TDevice.GetDeviceProperty(DeviceInformation: TDeviceInfoData;
  PropertyCode: integer; DeviceInfoSet: THandle; ValueKind: TValueKind): PByte;
var
  dummy: Cardinal; //variable for the function call
  dataType: Cardinal; //registry data type
  bufferSize: Cardinal; //buffer size
begin
  bufferSize := GetPropertyBufferSize(ValueKind);
  Result := AllocMem(bufferSize);
  //getting device property
  if not SetupDiGetDeviceRegistryProperty(DeviceInfoSet, DeviceInformation, PropertyCode,
    dataType, Result, bufferSize, dummy)
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end;
end; //GetDeviceProperty

//This function retutns the handle to the device information set for the
//specified device class
function TDevice.GetDeviceInformationSet(ClassGUID: TGUID): THandle;
begin
  //get all devices of the specified class
  Result := SetupDiGetClassDevsA(@ClassGUID, nil, 0, DIGCF_PRESENT or
    DIGCF_DEVICEINTERFACE);
  if Result = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end;
end; //GetDeviceInformationSet

//In this function we get the specified device information by its index in the
//device information set
function TDevice.GetDeviceInformation(DeviceNumber: TStorageDeviceNumber;
  ClassGUID: TGUID; DeviceInfoSet: THandle): TDeviceInfoData;
var
  i: integer; //current index
  deviceData: TSPDevInfoData; //device info data
  deviceInterfaceData: TSPDeviceInterfaceData; //device interface handle data
  deviceInterfaceDetailData: TSPDeviceInterfaceDetailDataA; //interface detail
  dwSize, bufSize: Cardinal; //buffer size
  fHandle: THandle; //file-device handle
  found: boolean; //boolean flag for stopping the search
  buffer: TStorageDeviceNumber; //buffer for device number
begin
  i := 0;
  found := false;
  deviceInterfaceData.cbSize := sizeof(deviceInterfaceData);
  deviceInterfaceData.InterfaceClassGuid := ClassGUID;
  //enumeration device interfaces
  while (SetupDiEnumDeviceInterfaces(DeviceInfoSet, nil, ClassGUID, i,
    deviceInterfaceData)) and (not found) do
  begin
    //preparations for function call
    deviceData.cbSize := sizeof(deviceData);
    deviceInterfaceDetailData.cbSize := 5;
    bufSize := sizeof(deviceInterfaceDetailData);
    //getting device number
    if not SetupDiGetDeviceInterfaceDetailA(DeviceInfoSet, @deviceInterfaceData,
      @deviceInterfaceDetailData, bufSize, dwSize, @deviceData)
    then begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError));
    end //then - exception
    else begin
      //check if this is a floppy drive
      if Pos(FLOPPY_DRIVE, deviceInterfaceDetailData.DevicePath) <> 1
      then begin
        //opening device as file
        fHandle := CreateFile(PChar(@deviceInterfaceDetailData.DevicePath[0]),
          GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
        if fHandle = INVALID_HANDLE_VALUE
        then begin
          raise EDeviceException.Create(SysErrorMessage(GetLastError));
        end //then - invalid file handle
        else begin
          //checking for device number matches
          try
            buffer := GetDeviceNumber(fHandle);
            if (buffer.DeviceNumber = DeviceNumber.DeviceNumber) and
              (buffer.PartitionNumber = DeviceNumber.PartitionNumber)
            then begin
              Result := deviceData;
              found := true;
            end; //then - found exact match
          finally
            CloseHandle(fHandle);
          end;
        end; //else - successfully opened
      end; //then - not a floppy
      Inc(i);
    end; //all ok
  end; //while
end; //GetDeviceInformation

//This function gets bus type of the specified device
function TDevice.GetBusType(Path: string): TBusType;
var
  handle: THandle; //file handle
  deviceDescriptor: STORAGE_DEVICE_DESCRIPTOR; //device descriptor
  dummy: Cardinal; //bytes returned by function
  propertyQuery: STORAGE_PROPERTY_QUERY; //property query
begin
  Result := BusTypeUnknown;
  handle := CreateFile(PChar(@Path[1]),GENERIC_READ, FILE_SHARE_READ or
    FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if handle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then - failed
  else begin
    try
      ZeroMemory(@deviceDescriptor, sizeof(deviceDescriptor));
      ZeroMemory(@propertyQuery, sizeof(propertyQuery));
      deviceDescriptor.Size := sizeof(deviceDescriptor);
      //we call DeviceIOControl to get info about the device
      if not DeviceIoControl(handle, IOCTL_STORAGE_QUERY_PROPERTY, @propertyQuery,
        sizeof(propertyQuery), @deviceDescriptor, sizeof(deviceDescriptor),
        dummy, nil)
      then begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError));
      end //then - failed to get information
      else begin
        Result := deviceDescriptor.BusType;
      end; //else - got device bus type
    finally
      CloseHandle(handle);
    end; //finally
  end; //else - successfully opened file
end; //GetBusType

//This function gets the device information by its instance handle in the device
//information set
function TDevice.GetDeviceInformation(InstanceHandle: THandle;
      DeviceInfoSet: THandle): TDeviceInfoData;
var
  deviceID: TCharArray; //buffer for the device ID
begin
  if CM_Get_Device_IDA(InstanceHandle, PChar(@deviceID[0]),
    sizeof(deviceID), 0) <> 0
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then - getting ID failed
  else begin
    //getting device information by its ID
    Result.cbSize := sizeof(Result);
    if not SetupDiOpenDeviceInfoA(DeviceInfoSet, PChar(@deviceID[0]), 0,
      0, @Result)
    then begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError));
    end; //then - failed to get DeviceInfo
  end; //else - successfully got ID
end; //GetDeviceInformation

//This procedure gets all about the device throug SetupAPI
procedure TDevice.GetSetupAPIInforamtion(ClassGUID: TGUID);
begin
  //Bus type is not applicable to volumes, so it's called in another class
  fBusType := BusTypeUnknown;
  fCapabilities := TDeviceCapabilities(GetDeviceProperty(fDeviceInfoData,
    SPDRP_CAPABILITIES, fDeviceInfoSet, vkNumber)^);
  fChildren := TList.Create;
  fClassGUID := ClassGUID;
  fDescription := PChar(GetDeviceProperty(fDeviceInfoData, SPDRP_DEVICEDESC,
    fDeviceInfoSet, vkString));
  fDeviceClassName := PChar(GetDeviceProperty(fDeviceInfoData, SPDRP_CLASS,
    fDeviceInfoSet, vkString));
  //Friendly name is not applicable to all kinds of device
  fManufacturer := PChar(GetDeviceProperty(fDeviceInfoData, SPDRP_MFG,
    fDeviceInfoSet, vkString));
end; //GetSetupAPIInformation

{CONSTRUCTOR}
constructor TDevice.Create(ClassGUID: TGUID; Path: string);
var
  fileHandle: THandle; //device file handle
begin
  inherited Create;
  //we open device as file
  fPath := FormatDevicePath(Path);
  fileHandle := CreateFile(PChar(@fPath[1]), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  //opening failed
  if fileHandle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then - failed
  else begin
    try
      //Here we get device information
      fDeviceInfoSet := GetDeviceInformationSet(ClassGUID);
      fDeviceNumber := GetDeviceNumber(fileHandle);
      fDeviceInfoData := GetDeviceInformation(fDeviceNumber, ClassGUID, fDeviceInfoSet);
      //Getting device properties
      GetSetupAPIInforamtion(ClassGUID);
    finally
      CloseHandle(fileHandle);
    end;
  end; //else - success
end; //Create

{CONSTRUCTOR OVERLOAD}
constructor TDevice.Create(ClassGUID: TGUID; InstanceHandle: Cardinal);
begin
  try
    fDeviceInfoSet := GetDeviceInformationSet(ClassGUID);
    fDeviceInfoData := GetDeviceInformation(InstanceHandle, fDeviceInfoSet);
    GetSetupAPIInforamtion(ClassGUID);
  finally
  end; //finally
end; //Create

{DESTRUCTOR}
destructor TDevice.Destroy;
var
  i: integer;
begin
  if fDeviceInfoSet <> INVALID_HANDLE_VALUE
  then begin
    SetupDiDestroyDeviceInfoList(fDeviceInfoSet);
  end;
  if Assigned(fChildren)
  then begin
    for i := 0 to fChildren.Count-1 do
    begin
      TDevice(fChildren.Items[i]).Destroy;
    end;
    fChildren.Free;
  end;
  inherited Destroy;
end; //Destroy

end.








