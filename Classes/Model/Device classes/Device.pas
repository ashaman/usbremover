{
  Device data class
  Contains definition of TDevice class which represents all the basic info about
  device.
  Copyright JLB Industries and Wikuim software, 2009
  Finished 4.11.2009
  This program is FREEWARE.
}
unit Device;

interface
uses
  WinIOCtl;
type
  {all kinds of devices defined in WinAPI enums}
  TBusType = (btSCSI, btATAPI, btATA, btFireWire, btSSA, btFibre, btUSB,
  btRAID, btiSCSI, btSAS, btSATA, btUnknown);

  //CLASS-WRAPPER FOR DEVICE
  TDevice = class(TObject)
  private
    fPath: PChar; {device path}
    fNumber: Integer; {number of drive in system}
    fVolumeLabel: PChar; {volume label}
    fVolumeID: longword; {volume ID}
    fBusType: TBusType; {device bus type}
    fVendorID: PChar; {vendor ID}
    fProductID: PChar; {product ID}
    fProductRevision: PChar; {product revision}
    fSize: Int64; {volume size}
    fFileSystemType: PChar; {file system type on the device}
    procedure GetAPIDeviceDescription(fHandle: THandle);
    procedure GetAPIVolumeInformation(fHandle: THandle);
    function APICharArrayToPChar(DeviceDescriptor: STORAGE_DEVICE_DESCRIPTOR; offset: Cardinal): PChar;
    {getters}
    function GetPath: PChar;
    function GetVolumeLabel: PChar;
    function GetBusType: TBusType;
    function GetVendorID: PChar;
    function GetProductRevision: PChar;
    function GetProductID: PChar;
    function GetVolumeSize: Int64;
    function GetFileSystemType: PChar;
    function GetVolumeID: longword;
    function GetDriveNumber: Integer;
    {end getters}
    class function StorageBusTypeToTBusType(sBusType: STORAGE_BUS_TYPE): TBusType;
  public
    constructor Create(path: PChar; index: integer);
    destructor Destroy; override;
    property Path: PChar read GetPath; {Root device directory}
    property VolumeLabel: PChar read GetVolumeLabel; {Volume label}
    property BusType: TBusType read GetBusType; {Bus type}
    property VendorID: PChar read GetVendorID; {Vendor ID}
    property ProductRevision: PChar read GetProductRevision; {Product revision}
    property ProductID: PChar read GetProductID; {Product ID}
    property VolumeSize: Int64 read GetVolumeSize; {Volume size}
    property FileSystemType: PChar read GetFileSystemType; {FS type}
    property VolumeID: longword read GetVolumeID; {volume ID}
    property DriveNumber: integer read GetDriveNumber; {physical drive number}
  end;

{==============================================================================}
implementation
uses
  Windows, DeviceException, SysUtils;
const
  MAXARRAYSIZE = 512;
  FLOPPY_DRIVE_1 = 'A';
  FLOPPY_DRIVE_2 = 'B';
type
  PCharArray = ^TCharArray; {pointer to char array}
  TCharArray = array [0..MAXARRAYSIZE-1] of char;

{This function gets the number of device in system}
function GetAPIDeviceNumber(handle: THandle): DWORD;
var
  deviceNumber: TStorageDeviceNumber; {structure for getting device storage number}
  Success: LongBool; {flag indicates the success (or fail) of the function call}
  dwBytesReturned: DWORD; {bytes returned by API functions}
begin
  Result := 0;
  ZeroMemory(@deviceNumber,sizeof(TStorageDeviceNumber));
  {query to device driver}
  Success := DeviceIoControl(handle, IOCTL_STORAGE_GET_DEVICE_NUMBER, nil,
    0, @deviceNumber, SizeOf(TStorageDeviceNumber), dwBytesReturned, nil);
  if not Success
  then begin
    EDeviceException.Create(SysErrorMessage(GetLastError));
  end {DeviceIoControl failed}
  else begin
    Result := deviceNumber.DeviceNumber;
  end;
  ZeroMemory(@deviceNumber,sizeof(TStorageDeviceNumber));
end;

procedure GetInfo(const driveLetter: char);
var
  hFile: THandle; {device file handle}
  dwBytesReturned: DWORD; {bytes returned by API functions}
  dwDeviceNumber: DWORD; {device number returned in TStorageDeviceNumber}
  flashGUID: TGUID; {flash drives GUID}
  hDevInfo: THandle;
begin
  {we open the volume as file}
  hFile := CreateFile(PChar(Format(VolumeMask, [driveLetter])), 0,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  try
    if hFile = INVALID_HANDLE_VALUE
    then begin
      EDeviceException.Create(SysErrorMessage(GetLastError));
    end {system exception}
    else begin
      dwDeviceNumber := GetAPIDeviceNumber(hFile);
      flashGUID := GUID_DEVINTERFACE_DISK;
      hDevInfo := SetupDiGetClassDevsA(@flashGUID, nil, 0, DIGCF_PRESENT or
        DIGCF_DEVICEINTERFACE);
      if hDevInfo = INVALID_HANDLE_VALUE then
      begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError));
      end;
    end; {successful initialization}
  finally
    try
      CloseHandle(hFile);
    except {exception supression}
    end;
  end; {finally}
end;

{Converts char arrays to PChar}
function TDevice.APICharArrayToPChar(DeviceDescriptor: STORAGE_DEVICE_DESCRIPTOR;
  offset: Cardinal): PChar;
var
  buf: PChar; {string for buffer}
begin
  Result := '';
  if offset <> 0
  then begin
    buf := @PCharArray(@DeviceDescriptor)^[offset];
    Result := PChar(Trim(buf));
  end;
end;

{Gets device description from STORAGE_DEVICE_DESCRIPTOR structure}
procedure TDevice.GetAPIDeviceDescription(fHandle: THandle);
var
  DeviceDescriptor: STORAGE_DEVICE_DESCRIPTOR; {system device descriptor}
  ReturnedBytes: DWORD; {buffer for returned bytes}
  PropQuery: STORAGE_PROPERTY_QUERY; {property query}
  Success: LongBool; {indicates if a function call was successful}
  hDev: THandle;
  DskSize: GET_LENGTH_INFORMATION;
begin
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
      raise EDeviceException.Create(SysErrorMessage(GetLastError));
    end {exception - no such device}
    else begin
      if (DeviceDescriptor.BusType = BusTypeUnknown) or
        (DeviceDescriptor.BusType = BusTypeMaxReserved)
      then begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError)); //unknown bus type
      end {unknown bus type}
      else begin
        {Converting bus type to TBusType}
        fBusType := TDevice.StorageBusTypeToTBusType(DeviceDescriptor.BusType);
        {getting disk size}
        {

        LINKS
        http://msdn.microsoft.com/en-us/library/aa510280.aspx
        http://msdn.microsoft.com/en-us/library/aa363147%28VS.85%29.aspx
        http://msdn.microsoft.com/en-us/library/aa363215%28VS.85%29.aspx
        http://msdn.microsoft.com/en-us/library/aa363432%28VS.85%29.aspx
        http://msdn.microsoft.com/en-us/library/aa363427%28VS.85%29.aspx
        http://msdn.microsoft.com/en-us/library/aa363431%28VS.85%29.aspx
        http://forum.sources.ru/index.php?showtopic=146127
        http://msdn.microsoft.com/en-us/library/cc526325.aspx
        http://forum.shelek.ru/index.php/topic,4477.0.html

          also get media serial -
            through MEDIA_SERIAL_NUMBER_DATA and
            IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER

          fNumber - get through DeviceIoControl(
  (HANDLE) hDevice,                        // handle to device
  IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,    // dwIoControlCode
  NULL,                                    // lpInBuffer
  0,                                       // nInBufferSize
  (LPVOID) lpOutBuffer,                    // output buffer
  (DWORD) nOutBufferSize,                  // size of output buffer
  (LPDWORD) lpBytesReturned,               // number of bytes returned
  (LPOVERLAPPED) lpOverlapped              // OVERLAPPED structure
);
        hDev := CreateFile(PChar('\\\\.\\PhysicalDrive2'),0,FILE_SHARE_READ or
          FILE_SHARE_WRITE,nil, OPEN_EXISTING,0, 0);
        DeviceIoControl(hDev,  // device to be queried
      IOCTL_DISK_GET_LENGTH_INFO,  // operation to perform
                             nil, 0, // no input buffer
                            @DskSize, sizeof(DskSize),     // output buffer
                            ReturnedBytes,                 // # bytes returned
                            nil);
      fSize := DskSize.Length;
      //NOW IT WORKS... BUT VERY STRANGE...
      }
        fSize := DiskSize(ord(fPath[0])-ord(FLOPPY_DRIVE_1)+1);
        {Getting vendor ID}
        fVendorID := self.APICharArrayToPChar(DeviceDescriptor,
          DeviceDescriptor.VendorIdOffset);
        {Getting product revision}
        fProductRevision := self.APICharArrayToPChar(DeviceDescriptor,
          DeviceDescriptor.ProductRevisionOffset);
        {Getting product ID}
        fProductID := self.APICharArrayToPChar(DeviceDescriptor,
          DeviceDescriptor.ProductIdOffset);
      end;
    end;
    ZeroMemory(@PropQuery, sizeof(PropQuery));
    ZeroMemory(@DeviceDescriptor, sizeof(DeviceDescriptor));
end;

{Gets volume information}
procedure TDevice.GetAPIVolumeInformation(fHandle: THandle);
var
  VolumeNameBuf, FileSystemNameBuffer: TCharArray; {buffers for strings}
  Success: LongBool; {indicates if a function call was successful}
  ReturnedBytes: DWORD; {buffer for returned bytes}
  FSFlags: DWORD; {File system flags set for device}
begin
  {Getting volume information}
  Success := GetVolumeInformation(fPath,VolumeNameBuf,MAXCHAR,
    @fVolumeID,ReturnedBytes,FSFlags,FileSystemNameBuffer,MAXCHAR);
  if not Success
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end {cannot get volume info}
  else begin
    {setting volume label and FS type}
    fVolumeLabel := PChar(@VolumeNameBuf);
    fFileSystemType := PChar(@FileSystemNameBuffer);
  end;
end;

{Class constructor. Gets info about device}
constructor TDevice.Create(path: PChar; index: integer);
var
  fHandle: THandle; {device file handle}
begin
  inherited Create;
  {here we save root drive path}
  fPath := path;
  fHandle := CreateFile(PChar('\\.\'+path[0]+':'),GENERIC_READ or
    GENERIC_WRITE,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,
    OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  try
    //we open new drive as file
    if fHandle = INVALID_HANDLE_VALUE {invalid handle}
    then begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError)); {opening failed}
    end {exception - invalid file}
    else begin
      {getting all necessary info about volume}
      GetAPIDeviceDescription(fHandle);
      GetAPIVolumeInformation(fHandle);
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
  Getters and setters
}

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

function TDevice.GetDriveNumber: Integer;
begin
  Result := fNumber;
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
