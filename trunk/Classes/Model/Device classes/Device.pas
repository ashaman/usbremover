{
  Device data class
  Contains definition of TDevice class which represents all the basic info about
  device.
  Copyright JLB Industries and Wikuim software, 2009
  This program is FREEWARE.
}
unit Device;

interface
uses
  WinIOCtl;
const
  FLOPPY_DRIVE_1 = 'A';
  FLOPPY_DRIVE_2 = 'B';

type
  {all kinds of devices defined in WinAPI enums}
  TBusType = (btSCSI, btATAPI, btATA, btFireWire, btSSA, btFibre, btUSB,
  btRAID, btiSCSI, btSAS, btSATA, btUnknown);

  //CLASS-WRAPPER FOR DEVICE
  TDevice = class(TObject)
  private
    fHandle: THandle; {handle for device}
    fPath: PChar; {device path}
    fVolumeLabel: PChar; {volume label}
    fVolumeID: longword; {volume ID}
    fBusType: TBusType; {device bus type}
    fVendorID: PChar; {vendor ID}
    fProductID: PChar; {product ID}
    fProductRevision: PChar; {product revision}
    fSize: Int64; {volume size}
    fFileSystemType: PChar; {file system type on the device}
    function GetHandle: THandle;
    function GetPath: PChar;
    function GetVolumeLabel: PChar;
    function GetBusType: TBusType;
    function GetVendorID: PChar;
    function GetProductRevision: PChar;
    function GetProductID: PChar;
    function GetVolumeSize: Int64;
    function GetFileSystemType: PChar;
    function GetVolumeID: longword;
    class function StorageBusTypeToTBusType(sBusType: STORAGE_BUS_TYPE): TBusType;
  public
    constructor Create(path: PChar; index: integer);
    destructor Destroy; override;
    property Handle: THandle read GetHandle; {Device handle}
    property Path: PChar read GetPath; {Root device directory}
    property VolumeLabel: PChar read GetVolumeLabel; {Volume label}
    property BusType: TBusType read GetBusType; {Bus type}
    property VendorID: PChar read GetVendorID; {Vendor ID}
    property ProductRevision: PChar read GetProductRevision; {Product revision}
    property ProductID: PChar read GetProductID; {Product ID}
    property VolumeSize: Int64 read GetVolumeSize; {Volume size}
    property FileSystemType: PChar read GetFileSystemType; {FS type}
    property VolumeID: longword read GetVolumeID;
  end;

{==============================================================================}
implementation
uses
  Windows, DeviceException, SysUtils, magwmi;
const
  MAXARRAYSIZE = 512;

type
  //type definition for device descriptor
  STORAGE_DEVICE_DESCRIPTOR = packed record
    Version: ULONG;
    Size: ULONG;
    DeviceType: UCHAR;
    DeviceTypeModifier: UCHAR;
    RemovableMedia: boolean;
    CommandQueueing: boolean;
    VendorIdOffset: ULONG;
    ProductIdOffset: ULONG;
    ProductRevisionOffset: ULONG;
    SerialNumberOffset: ULONG;
    BusType: STORAGE_BUS_TYPE;
    RawPropertiesLength: ULONG;
    RawDeviceProperties: array[0..511] of byte;
  end;

  //Type definition for device query
  STORAGE_PROPERTY_QUERY = packed record
    PropertyId: DWORD;
    QueryType: DWORD;
    AdditionalParameters: PByte;
  end;

  {
    ****UNUSED SECTION
    Reserved for potential future use

  //Type definition for getting physical drive number
  TDiskExtent = record
    DiskNumber: DWORD;
    StartingOffset: LARGE_INTEGER;
    ExtentLength: LARGE_INTEGER;
  end;
  PDiskExtent = ^TDiskExtent;

  //Type definition for getting physical drive number
  TVolumeDiskExtents = record
    NumberOfDiskExtents: DWORD;
    Extents: array[0..1] of TDiskExtent;
  end;
  PVolumeDiskExtents = ^TVolumeDiskExtents;

  //Drive Length information
  GET_LENGTH_INFORMATION = record
    Length: integer;
  end;
  }

  //info about serial number
  MEDIA_SERIAL_NUMBER_DATA= packed record
    SerialNumberLength:Cardinal;
    Result:Cardinal;
    AuthCommand:Cardinal;
    Reserved:Cardinal;
    SerialNumberData:Byte;
   end;

{
  Class constructor
  Gets info about device
}
constructor TDevice.Create(path: PChar; index: integer);
type
  PCharArray = ^TCharArray; {pointer to char array}
  TCharArray = array [0..MAXARRAYSIZE-1] of char;
var
  DeviceDescriptor: STORAGE_DEVICE_DESCRIPTOR; {system device descriptor}
  PropQuery: STORAGE_PROPERTY_QUERY; {property query}
  //DiskExtent: TVolumeDiskExtents; {disk extent variable}
  //LengthInfo: GET_LENGTH_INFORMATION; {length information}
  ReturnedBytes: DWORD; {buffer for returned bytes}
  Success: LongBool; {indicates if a function call was successful}
  FSFlags: DWORD; {File system flags set for device}
  VolumeNameBuf, FileSystemNameBuffer: TCharArray; {buffers for strings}
  buf: PChar; {string for buffer}
begin
  inherited Create;
  {here we save root drive path}
  fPath := path;
  try
    //we open new drive as file
    fHandle := CreateFile(PChar('\\.\'+path[0]+':'),GENERIC_READ or
      GENERIC_WRITE,FILE_SHARE_READ or
      FILE_SHARE_WRITE,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
    if fHandle = INVALID_HANDLE_VALUE {invalid handle}
    then begin
      raise EDeviceException.Create('Invalid volume handle!'); {opening failed}
    end {exception - invalid file}
    else begin
      {there we get volume size}
      //zero memofy for pointers
      ZeroMemory(@PropQuery, sizeof(PropQuery));
      ZeroMemory(@DeviceDescriptor, sizeof(DeviceDescriptor));
      //getting size of structure
      DeviceDescriptor.Size := sizeof(DeviceDescriptor);
      //we call DeviceIOControl to get info about the device
      //Returned bytes has no meaning in this case
      Success := DeviceIoControl(fHandle,IOCTL_STORAGE_QUERY_PROPERTY,@PropQuery,
        sizeof(PropQuery), @DeviceDescriptor, DeviceDescriptor.Size,
        ReturnedBytes, nil);
      if not Success
      then begin
        raise EDeviceException.Create('Device does not exist!'); //DeviceIOControl
      end {exception - no such device}                           //failed
      else begin
        if success then
        if (DeviceDescriptor.BusType = BusTypeUnknown) or
          (DeviceDescriptor.BusType = BusTypeMaxReserved)
        then begin
          raise EDeviceException.Create('Unknown bus type!'); //unknown bus type
        end {unknown bus type}
        else begin
          {Converting bus type to TBusType}
          fBusType := TDevice.StorageBusTypeToTBusType(DeviceDescriptor.BusType);
          {Getting volume information}
          Success := GetVolumeInformation(fPath,VolumeNameBuf,MAXCHAR,
            @fVolumeID,ReturnedBytes,FSFlags,FileSystemNameBuffer,MAXCHAR);
          if not Success
          then begin
            raise EDeviceException.Create('Unknown volume!');
          end {cannot get volume info}
          else begin
            {setting volume label and FS type}
            fVolumeLabel := PChar(@VolumeNameBuf);
            fFileSystemType := PChar(@FileSystemNameBuffer);

            {
              UNUSED SECTION: maybe used in the next releases

              //we get the number of drive in system
              ZeroMemory(@DiskExtent,sizeof(DiskExtent));
              Success := DeviceIoControl(fHandle,IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
                nil,0,@DiskExtent,sizeof(DiskExtent),ReturnedBytes,
                nil);
            }

            {getting disk size}
            fSize := DiskSize(ord(fPath[0])-ord(FLOPPY_DRIVE_1)+1);
            {Getting vendor ID}
            if DeviceDescriptor.VendorIdOffset <> 0
            then begin
              buf := @PCharArray(@DeviceDescriptor)^[DeviceDescriptor.VendorIdOffset];
              fVendorID := PChar(Trim(buf));
            end;
            {Getting product revision}
            if DeviceDescriptor.ProductRevisionOffset <> 0
            then begin
              buf := @PCharArray(@DeviceDescriptor)^[DeviceDescriptor.ProductRevisionOffset];
              fProductRevision := PChar(Trim(buf));
            end;
            {Getting product ID}
            if DeviceDescriptor.ProductIdOffset <> 0
            then begin
              buf := @PCharArray(@DeviceDescriptor)^[DeviceDescriptor.ProductIdOffset];
              fProductID := PChar(Trim(buf));
            end;
          end; {got volume info}
        end; {bus inited successfully}
      end; {success - device found}
    end; {success - file created}
  finally
    if fHandle <> INVALID_HANDLE_VALUE
    then begin
      CloseHandle(fHandle); {we release resources}
      ZeroMemory(@PropQuery, sizeof(PropQuery));
      ZeroMemory(@DeviceDescriptor, sizeof(DeviceDescriptor));
    end; {close handle}
  end;
end;

destructor TDevice.Destroy;
begin
  inherited Destroy;
end;

{
  Getters and setters
}
function TDevice.GetHandle: THandle;
begin
  Result := fHandle;
end;

function TDevice.GetPath: PChar;
begin
  Result := fPath;
end;

function TDevice.GetVolumeLabel;
begin
  Result := fVolumeLabel;
end;

function TDevice.GetBusType: TBusType;
begin
  Result := fBusType;
end;

function TDevice.GetVendorID: PChar;
begin
  Result := fVendorID;
end;

function TDevice.GetProductRevision: PChar;
begin
  Result := fProductRevision;
end;

function TDevice.GetProductID: PChar;
begin
  Result := fProductID;
end;

function TDevice.GetVolumeSize: Int64;
begin
  Result := fSize;
end;

function TDevice.GetFileSystemType: PChar;
begin
  Result := fFileSystemType;
end;

function TDevice.GetVolumeID: longword;
begin
  Result := fVolumeID;
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
