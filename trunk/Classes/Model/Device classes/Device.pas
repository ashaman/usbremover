{
  Device data class
  Contains definition of TDevice class which represents all the basic info about
  device.
  Copyright JLB Industries and Wikuim software, 2009
  This program is FREEWARE.

  TODO: use WMI functions
  REPAIR DRIVE EJECTION!!!
  REMOVE PChars and add strings!!!
}
unit Device;

interface
uses
  WinIOCtl, Classes, WbemScripting_TLB, Volume;

type
  {all kinds of devices defined in WinAPI enums}
  TBusType = (btSCSI, btATAPI, btATA, btFireWire, btSSA, btFibre, btUSB,
  btRAID, btiSCSI, btSAS, btSATA, btUnknown);

  //CLASS-WRAPPER FOR DEVICE
  TDevice = class(TObject)
  private
    fDeviceID: PChar; //device ID
    fDeviceNumber: Cardinal; //device number in system
    fDeviceNumberOfPartitions: integer; //number of drive partitions
    fDeviceManufacturer: PChar; //device manufacturer
    fDeviceModel: PChar; //device model
    fDeviceBusType: PChar; //device bus type
    fDeviceName: PChar; //device name
    fDeviceVolume: TVolume; //device volume
    //These functions get disk partition information and its device number
    class function GetAPIDeviceNumber(fHandle: THandle): Cardinal;
    procedure GetAPIDeviceDescription(devNumber: Cardinal);
    procedure SetDeviceProperty(devProperty: ISWbemProperty);
    {getters}
    function GetDeviceID: PChar;
    function GetDeviceNumber: Cardinal;
    function GetDeviceNumberOfPartitions: Integer;
    function GetDeviceManufacturer: PChar;
    function GetDeviceModel: PChar;
    function GetBusType: PChar;
    function GetDeviceName: PChar;
    {end getters}
  public
    constructor Create(path: PChar);
    destructor Destroy; override;
    //properties
    property DeviceID: PChar read GetDeviceID; {Device ID}
    property DeviceNumber: Cardinal read GetDeviceNumber; {physical drive number}
    property DeviceNumberOfPartitions: integer read GetDeviceNumberOfPartitions; {number of partitions}
    property DeviceManufacturer: PChar read GetDeviceManufacturer; {device manufacturer}
    property DeviceModel: PChar read GetDeviceModel; {Model}
    property DeviceBusType: PChar read GetBusType; {Bus type}
    property DeviceName: PChar read GetDeviceName; {Name}
    //property Items[index: integer]: TVolume;
  end;

{==============================================================================}
implementation
uses
  Windows, DeviceException, SysUtils, WMI, OleServer, ActiveX;

//This function gets device number
class function TDevice.GetAPIDeviceNumber(fHandle: THandle): Cardinal;
var
  Success: LongBool; //boolean flag
  devNumber: TStorageDeviceNumber; //structure which describes device number
  ReturnedBytes: Cardinal; //bytes returned by DeviceIoControl
begin
  //call DeviceIOControl
  Success := DeviceIoControl(fHandle,IOCTL_STORAGE_GET_DEVICE_NUMBER,
    nil, 0, @devNumber, sizeof(TStorageDeviceNumber), ReturnedBytes, nil);
  if not Success
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //getting device number failed
  else begin
    Result := devNumber.DeviceNumber;
  end; //successful
end;

//gets property name and sets the value
procedure TDevice.SetDeviceProperty(devProperty: ISWbemProperty);
var
  buf: TStringList; //buffer string
begin

  {
    CLONE VALUES!!!
  }
  //string Caption;
  //string Description;
  if devProperty.Name = 'DeviceID'
  then begin
    fDeviceID := PChar(@string(devProperty.Get_Value)[1]);
    exit;
  end;
  //string FirmwareRevision; not available on XP
  if devProperty.Name = 'InterfaceType'
  then begin
    fDeviceBusType := PChar(@string(devProperty.Get_Value)[1]);
    exit;
  end;
  if devProperty.Name = 'Manufacturer'
  then begin
    fDeviceManufacturer := PChar(@string(devProperty.Get_Value)[1]);
    exit;
  end;
  if devProperty.Name = 'Model'
  then begin
    fDeviceModel := PChar(string(devProperty.Get_Value));
    exit;
  end;
  if devProperty.Name = 'Name'
  then begin
    fDeviceName := PChar(string(devProperty.Get_Value));
    exit;
  end;
  if devProperty.Name = 'Partitions'
  then begin
    fDeviceNumberOfPartitions := Cardinal(devProperty.Get_Value);
    exit;
  end;
  if devProperty.Name = 'PNPDeviceID'
  then begin
    buf := TStringList.Create;
    ExtractStrings(['\','&'],[], PChar(string(devProperty.Get_Value)), buf);
    exit;
  end;
  //string SerialNumber; not supported on XP
  //uint64 Size;
end;

{Gets device description from WMI STORAGE_DEVICE_DESCRIPTOR structure}
procedure TDevice.GetAPIDeviceDescription(devNumber: Cardinal);
var
  Locator: TSWbemLocator; //OLE provider
  Services: ISWbemServices; //namespace objects
  ObjectSet:  ISWbemObjectSet; //query result - set of objects
  ObjectSetItem: ISWbemObject; //item of the ObjectSet
  Enumerator: IEnumVariant; //device enumerator
  PropertyEnumerator: IEnumVariant; //device properties enumerator
  PropertySet: ISWbemPropertySet; //device properties set
  OleObject: OleVariant; //buffer for getting device info
  DeviceProperty: ISWbemProperty; //device property
  ReturnNumber: Cardinal; //return parameter of procedure call
begin
  //we create OLE provider
  Locator := TSWbemLocator.Create(nil);
  //we connect to the local computer namespace
  Services := Locator.ConnectServer('.','root\CIMV2','','','','',0,nil);
  //we execute query to get the only device we need
  ObjectSet := Services.ExecQuery('SELECT * FROM Win32_DiskDrive WHERE DeviceID="'+
    Format(DrivePattern,[devNumber])+'"','WQL',wbemFlagReturnImmediately
    and wbemFlagForwardOnly, nil);
  //we get the device enumerator
  Enumerator := (ObjectSet._NewEnum) as IEnumVariant;
  //we iterate through items
  while (Enumerator.Next(1,OleObject,ReturnNumber) = S_OK) do
  begin
    //we get device...
    ObjectSetItem := IUnknown(OleObject) as SWbemObject;
    //...then, get property set...
    PropertySet := ObjectSetItem.Properties_;
    //...and get properties iterator
    PropertyEnumerator := (PropertySet._NewEnum) as IEnumVariant;
    //here we iterate over device properties
    while (PropertyEnumerator.Next(1,OleObject,ReturnNumber) = S_OK) do
    begin
      DeviceProperty := IUnknown(OleObject) as SWbemProperty;
      self.SetDeviceProperty(DeviceProperty);
    end; //while for properties
  end; //while for devices
end; //procedure

{Class constructor. Gets info about device}
constructor TDevice.Create(path: PChar);
var
  fHandle: THandle; //device file handle
begin
  inherited Create;
  //we open first device logical partition as file

  {
    CM_Locate_DevNode

    CM_Get_DevNode_Status - get device Запоминающее устройство для USB

    CM_Get_DevNode_Registry_Property

    CM_Get_Child
  }

  fHandle := CreateFile(PChar(Format(VolumeMask,[path[0]])),0,
    FILE_SHARE_READ or FILE_SHARE_WRITE,nil,
    OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  try
    if fHandle = INVALID_HANDLE_VALUE {invalid handle}
    then begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError)); {opening failed}
    end //exception - invalid file
    else begin
      //getting device number
      fDeviceNumber := GetAPIDeviceNumber(fHandle);
      //getting all necessary info about volume
      GetAPIDeviceDescription(fDeviceNumber);
      //setting the first volume on device
      fDeviceVolume := TVolume.Create(path);
    end; //success - file created
  finally
    try
      CloseHandle(fHandle);
    except //supress exception
    end;
  end;
end;

destructor TDevice.Destroy;
var
  i: integer;
begin
  fDeviceVolume.Destroy;
  inherited Destroy;
end;

{
  GETTERS SECTION
}
function TDevice.GetBusType: PChar;
begin
  Result := fDeviceBusType;
end;

function TDevice.GetDeviceNumber: Cardinal;
begin
  Result := fDeviceNumber;
end;

function TDevice.GetDeviceID: PChar;
begin
  Result := fDeviceID;
end;

function TDevice.GetDeviceNumberOfPartitions: Integer;
begin
  Result := fDeviceNumberOfPartitions;
end;

function TDevice.GetDeviceManufacturer: PChar;
begin
  Result := fDeviceManufacturer;
end;

function TDevice.GetDeviceModel: PChar;
begin
  Result := fDeviceModel;
end;

function TDevice.GetDeviceName: PChar;
begin
  Result := fDeviceName;
end;

end.
