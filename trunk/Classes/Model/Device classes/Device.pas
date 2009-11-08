{
  Device data class
  Contains definition of TDevice class which represents all the basic info about
  device.
  Copyright JLB Industries and Wikuim software, 2009
  This program is FREEWARE.

  TODO: use WMI functions
}
unit Device;

interface
uses
  WinIOCtl, Classes;
type
  {all kinds of devices defined in WinAPI enums}
  TBusType = (btSCSI, btATAPI, btATA, btFireWire, btSSA, btFibre, btUSB,
  btRAID, btiSCSI, btSAS, btSATA, btUnknown);

  //CLASS-WRAPPER FOR DEVICE
  TDevice = class(TObject)
  private
    fDeviceNumber: Integer; //device number in system
    fDeviceBusType: TBusType; //device bus type
    fDeviceVendorID: PChar; //vendor ID
    fDeviceProductID: PChar; //product ID
    fDeviceProductRevision: PChar; //product revision
    fDeviceVolumes: TList; //all device volumes
    //These functions get disk partition information and its device number
    function GetAPIDeviceNumber(fHandle: THandle): Cardinal;
    procedure GetAPIDeviceDescription(devNumber: Cardinal);
    {getters}
    function GetBusType: TBusType;
    function GetVendorID: PChar;
    function GetProductRevision: PChar;
    function GetProductID: PChar;
    function GetDeviceNumber: Integer;
    {end getters}
    class function StorageBusTypeToTBusType(sBusType: STORAGE_BUS_TYPE): TBusType;
  public
    constructor Create(path: PChar);
    destructor Destroy; override;
    property DeviceBusType: TBusType read GetBusType; {Bus type}
    property DeviceVendorID: PChar read GetVendorID; {Vendor ID}
    property DeviceProductRevision: PChar read GetProductRevision; {Product revision}
    property DeviceProductID: PChar read GetProductID; {Product ID}
    property DeviceNumber: integer read GetDeviceNumber; {physical drive number}
  end;

{==============================================================================}
implementation
uses
  Windows, DeviceException, SysUtils, WMI, OleServer, WbemScripting_TLB, ActiveX;

//This function gets device number
function TDevice.GetAPIDeviceNumber(fHandle: THandle): Cardinal;
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

{Gets device description from WMI STORAGE_DEVICE_DESCRIPTOR structure}
procedure TDevice.GetAPIDeviceDescription(devNumber: Cardinal);
var
  Locator: TSWbemLocator;
  Services:   ISWbemServices;
  ObjectSet:  ISWbemObjectSet;
  ISObject: ISWbemObject;
  Enum, PropEnum: IEnumVariant;
  PropSet: ISWbemPropertySet;
  tmp: OleVariant;
  value: cardinal;
  SProp: ISWbemProperty;
  strval: string;
  {
  PnpHandle: HDEVINFO; //pointer to PnP disk drives info
  devData: TSPDevInfoData; //SP_DEVINFO_DATA - info about device
  Success: LongBool; //boolean flag
  ReturnedBytes: Cardinal; //bytes returned by function
  RegDataType: Cardinal;
  Buf: TCharArray;
  b2: DWORD;
  }
begin
  Locator := TSWbemLocator.Create(nil);
  Services := Locator.ConnectServer('.','root\CIMV2','','','','',0,nil);
  {заэскейпировать бэкслеши}
  ObjectSet := Services.ExecQuery('SELECT * FROM Win32_DiskDrive WHERE DeviceID="\\\\.\\PhysicalDrive'
    +IntToStr(devNumber-1)+'"','WQL',wbemFlagReturnImmediately and wbemFlagForwardOnly, nil);
  value := ObjectSet.Count;
  {
  ISObject := Services.Get('Win32_DiskDrive',wbemFlagUseAmendedQualifiers,nil);
  ObjectSet := ISObject.Instances_(wbemFlagBidirectional,nil);
  }
  Enum := (ObjectSet._NewEnum) as IEnumVariant;
  while (Enum.Next(1,tmp,value) = S_OK) do
  begin
    ISObject := IUnknown(tmp) as SWbemObject;
    PropSet := ISObject.Properties_;
    PropEnum := (PropSet._NewEnum) as IEnumVariant;

    while (PropEnum.Next(1,tmp,Value) = S_OK) do
    begin
      SProp := IUnknown(tmp) as SWbemProperty;
      strval := SProp.Name;
      if strval = 'PNPDeviceID' {Works only on Vista and higher!!!}
      then begin
        strval := SProp.Get_Value;
      end;
    end;
  end;
end;

{Class constructor. Gets info about device}
constructor TDevice.Create(path: PChar);
var
  fHandle: THandle; {device file handle}
  devNum: Cardinal;
begin
  inherited Create;
  fDeviceVolumes := TList.Create;
  //we open first device logical partition as file
  fHandle := CreateFile(PChar(Format(VolumeMask,[path[0]])),0,
    FILE_SHARE_READ or FILE_SHARE_WRITE,nil,
    OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  try
    if fHandle = INVALID_HANDLE_VALUE {invalid handle}
    then begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError)); {opening failed}
    end //exception - invalid file
    else begin
      devNum := self.GetAPIDeviceNumber(fHandle);
      //GetAPISerialNumber(devNum);
      {getting all necessary info about volume}
      GetAPIDeviceDescription(devNum);
    end; {success - file created}
  finally
    try
      CloseHandle(fHandle);
    except {supress exception}
    end;
  end;
end;

destructor TDevice.Destroy;
begin
  inherited Destroy;
end;

{
  GETTERS SECTION
}

function TDevice.GetBusType: TBusType;
begin
  Result := fDeviceBusType;
end;

function TDevice.GetVendorID: PChar;
begin
  Result := fDeviceVendorID;
end;

function TDevice.GetProductRevision: PChar;
begin
  Result := fDeviceProductRevision;
end;

function TDevice.GetProductID: PChar;
begin
  Result := fDeviceProductID;
end;

function TDevice.GetDeviceNumber: Integer;
begin
  Result := fDeviceNumber;
end;

//Converts integer codes to TBusType enumeration
class function TDevice.StorageBusTypeToTBusType(sBusType: STORAGE_BUS_TYPE): TBusType;
begin
  Result := btUnknown;
  case sBusType of
    BusTypeScsi: Result := btSCSI;
    BusTypeAtapi: Result := btATAPI;
    BusTypeAta: Result := btATA;
    BusType1394: Result := btFireWire;
    BusTypeSsa: Result := btSSA;
    BusTypeFibre: Result := btFibre;
    BusTypeUsb: Result := btUSB;
    BusTypeRAID: Result := btRAID;
    BusTypeiSCSI: Result := btiSCSI;
    BusTypeSas: Result := btSAS;
    BusTypeSata: Result := btSATA;
  end;
end;

end.
