{
  Partial translation of WinIOCtl.h, Dbt.h, setupapi.h and other Win SDK headers
}
unit WinIOCtl;

interface

uses
  Windows, SysUtils;

//Structure align for unpacked structures
{$ALIGN 8}

const
  DeviceMask = '%s:';
  VolumeMask = '\\.\' + DeviceMask;
  DrivePattern = '\\\\.\\PhysicalDrive%d';
  DEV_FLOPPY = '\Device\Floppy';
  FLOPPY_DRIVE_1 = 'A';
  FLOPPY_DRIVE_2 = 'B';

type
  TCharArray = array [0..MAX_PATH] of char;
  PCharArray = ^TCharArray;
{
  STRUCTURES AND CONSTANTS FROM WINIOCTL.H
  Partially translated by J.L.Blackrow (e-mail: DarthYarius_0990@mail.ru)
  Partially translated by Alexander Bagel
  Copyright : © Fangorn Wizards Lab 1998 - 2009.
}

const
  FILE_DEVICE_DISK = $00000007;
  FILE_DEVICE_MASS_STORAGE = $0000002d;
  FILE_DEVICE_FILE_SYSTEM = $00000009;

  METHOD_BUFFERED = 0;

  IOCTL_DISK_BASE = FILE_DEVICE_DISK;
  IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;

  //File access
  FILE_ANY_ACCESS = 0;
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  FILE_READ_ACCESS = $0001; // file & pipe
  FILE_WRITE_ACCESS = $0002; // file & pipe

  //IOCTL FLAGS
  IOCTL_DISK_GET_DRIVE_GEOMETRY  = (IOCTL_DISK_BASE shl 16) or ($0000 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_GET_PARTITION_INFO = (IOCTL_DISK_BASE shl 16) or ($0001 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_DISK_SET_PARTITION_INFO = (IOCTL_DISK_BASE shl 16) or ($0002 shl 2) or (METHOD_BUFFERED) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14);
  IOCTL_DISK_GET_DRIVE_LAYOUT = (IOCTL_DISK_BASE shl 16) or ($0003 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_DISK_SET_DRIVE_LAYOUT = (IOCTL_DISK_BASE shl 16) or ($0004 shl 2) or (METHOD_BUFFERED) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14);
  IOCTL_DISK_VERIFY = (IOCTL_DISK_BASE shl 16) or ($0005 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_FORMAT_TRACKS = (IOCTL_DISK_BASE shl 16) or ($0006 shl 2) or (METHOD_BUFFERED) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14);
  IOCTL_DISK_REASSIGN_BLOCKS = (IOCTL_DISK_BASE shl 16) or ($0007 shl 2) or (METHOD_BUFFERED) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14);
  IOCTL_DISK_PERFORMANCE = (IOCTL_DISK_BASE shl 16) or ($0008 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_IS_WRITABLE = (IOCTL_DISK_BASE shl 16) or ($0009 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_LOGGING = (IOCTL_DISK_BASE shl 16) or ($000a shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_FORMAT_TRACKS_EX = (IOCTL_DISK_BASE shl 16) or ($000b shl 2) or (METHOD_BUFFERED) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14);
  IOCTL_DISK_HISTOGRAM_STRUCTURE = (IOCTL_DISK_BASE shl 16) or ($000c shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_HISTOGRAM_DATA = (IOCTL_DISK_BASE shl 16) or ($000d shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_HISTOGRAM_RESET = (IOCTL_DISK_BASE shl 16) or ($000e shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_REQUEST_STRUCTURE = (IOCTL_DISK_BASE shl 16) or ($000f shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_REQUEST_DATA = (IOCTL_DISK_BASE shl 16) or ($0010 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_PERFORMANCE_OFF = (IOCTL_DISK_BASE shl 16) or ($0018 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_DISK_GET_LENGTH_INFO = (IOCTL_DISK_BASE shl 16) or ($0017 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);

  //MASS STORAGE DEVICES FLAGS
  IOCTL_STORAGE_CHECK_VERIFY = (IOCTL_STORAGE_BASE shl 16) or ($0200 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_CHECK_VERIFY2 = (IOCTL_STORAGE_BASE shl 16) or ($0200 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_MEDIA_REMOVAL = (IOCTL_STORAGE_BASE shl 16) or ($0201 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_EJECT_MEDIA = (IOCTL_STORAGE_BASE shl 16) or ($0202 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_LOAD_MEDIA = (IOCTL_STORAGE_BASE shl 16) or ($0203 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_LOAD_MEDIA2 = (IOCTL_STORAGE_BASE shl 16) or ($0203 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_RESERVE = (IOCTL_STORAGE_BASE shl 16) or ($0204 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_RELEASE = (IOCTL_STORAGE_BASE shl 16) or ($0205 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_FIND_NEW_DEVICES = (IOCTL_STORAGE_BASE shl 16) or ($0206 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_EJECTION_CONTROL = (IOCTL_STORAGE_BASE shl 16) or ($0250 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_MCN_CONTROL = (IOCTL_STORAGE_BASE shl 16) or ($0251 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_GET_MEDIA_TYPES = (IOCTL_STORAGE_BASE shl 16) or ($0300 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_GET_MEDIA_TYPES_EX = (IOCTL_STORAGE_BASE shl 16) or ($0301 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER = (IOCTL_STORAGE_BASE shl 16) or ($0304 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_GET_HOTPLUG_INFO = (IOCTL_STORAGE_BASE shl 16) or ($0305 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_SET_HOTPLUG_INFO = (IOCTL_STORAGE_BASE shl 16) or ($0306 shl 2) or (METHOD_BUFFERED) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14);
  IOCTL_STORAGE_RESET_BUS = (IOCTL_STORAGE_BASE shl 16) or ($0400 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_RESET_DEVICE = (IOCTL_STORAGE_BASE shl 16) or ($0401 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_BREAK_RESERVATION = (IOCTL_STORAGE_BASE shl 16) or ($0405 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_GET_DEVICE_NUMBER = (IOCTL_STORAGE_BASE shl 16) or ($0420 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_PREDICT_FAILURE = (IOCTL_STORAGE_BASE shl 16) or ($0440 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  IOCTL_STORAGE_READ_CAPACITY = (IOCTL_STORAGE_BASE shl 16) or ($0450 shl 2) or (METHOD_BUFFERED) or (FILE_READ_ACCESS shl 14);
  IOCTL_STORAGE_QUERY_PROPERTY = $2D1400;

  {
  GUID_DEVINTERFACE_DISK: TGUID =
    (D1:$53f56307; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
    }

type
  DEVICE_TYPE = DWORD;
  PStorageDeviceNumber = ^TStorageDeviceNumber;
  TStorageDeviceNumber = packed record
    DeviceType: DEVICE_TYPE;
    DeviceNumber: DWORD;
    PartitionNumber: DWORD;
  end;

  STORAGE_BUS_TYPE=
  (
    BusTypeUnknown       = $00,
    BusTypeScsi          = $01,
    BusTypeAtapi         = $02,
    BusTypeAta           = $03,
    BusType1394          = $04,
    BusTypeSsa           = $05,
    BusTypeFibre         = $06,
    BusTypeUsb           = $07,
    BusTypeRAID          = $08,
    BusTypeiSCSI         = $09,
    BusTypeSas           = $0A,
    BusTypeSata          = $0B,
    BusTypeMaxReserved   = $7F
  );


{
  CONSTANTS AND TYPES FROM NTDDSCSI.H
  Partially translated by Alexander Bagel
  Copyright : © Fangorn Wizards Lab 1998 - 2009.
}
const
  SCSI_IOCTL_DATA_IN = 1;
  SCSIOP_MECHANISM_STATUS = $BD;

type
  USHORT = Word;

  PSCSI_PASS_THROUGH_DIRECT = ^SCSI_PASS_THROUGH_DIRECT;
  _SCSI_PASS_THROUGH_DIRECT = {packed} record
    Length: USHORT;
    ScsiStatus: UCHAR;
    PathId: UCHAR;
    TargetId: UCHAR;
    Lun: UCHAR;
    CdbLength: UCHAR;
    SenseInfoLength: UCHAR;
    DataIn: UCHAR;
    DataTransferLength: ULONG;
    TimeOutValue: ULONG;
    DataBuffer: ULONG;
    SenseInfoOffset: ULONG;
    Cdb: array [0..15] of UCHAR;
  end;
  SCSI_PASS_THROUGH_DIRECT = _SCSI_PASS_THROUGH_DIRECT;

  TSCSIPassThroughDirectBuffer = record
    Header: SCSI_PASS_THROUGH_DIRECT;
    SenseBuffer: array [0..31] of UCHAR;
    DataBuffer: array [0..191] of UCHAR;
  end;         


{
  PARTIAL TRANSLATION OF OTHER MS SDK HEADERS
  Partially translated by J.L.Blackrow (by me :))
}
const
  IOCTL_VOLUME_LOGICAL_TO_PHYSICAL = $560020;
  IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS = $560000;

  FSCTL_LOCK_VOLUME = (FILE_DEVICE_FILE_SYSTEM shl 16) or (6 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  FSCTL_UNLOCK_VOLUME = (FILE_DEVICE_FILE_SYSTEM shl 16) or (7 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);
  FSCTL_DISMOUNT_VOLUME = (FILE_DEVICE_FILE_SYSTEM shl 16) or (8 shl 2) or (METHOD_BUFFERED) or (FILE_ANY_ACCESS shl 14);

  {Window messages}

  {A request to change the current configuration (dock or undock) has been canceled}
  DBT_CONFIGCHANGECANCELED = $0019;
  {The current configuration has changed, due to a dock or undock}
  DBT_CONFIGCHANGED = $0018;
  {A custom event has occurred}
  DBT_CUSTOMEVENT = $8006;
  {A device or piece of media has been inserted and is now available}
  DBT_DEVICEARRIVAL = $8000;
  {Permission is requested to remove a device or piece of media.
  Any application can deny this request and cancel the removal}
  DBT_DEVICEQUERYREMOVE = $8001;
  {A request to remove a device or piece of media has been canceled}
  DBT_DEVICEQUERYREMOVEFAILED = $8002;
  {A device or piece of media has been removed}
  DBT_DEVICEREMOVECOMPLETE = $8004;
  {A device or piece of media is about to be removed. Cannot be denied}
  DBT_DEVICEREMOVEPENDING = $8003;
  {A device-specific event has occurred}
  DBT_DEVICETYPESPECIFIC = $8005;
  {A device has been added to or removed from the system}
  DBT_DEVNODES_CHANGED = $0007;
  {Permission is requested to change the current configuration (dock or undock)}
  DBT_QUERYCHANGECONFIG = $0017;
  {The meaning of this message is user-defined}
  DBT_USERDEFINED = $FFFF;

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

  //Type definition for getting physical drive number
  TDiskExtent = record
    DiskNumber: DWORD;
    StartingOffset: LARGE_INTEGER;
    ExtentLength: LARGE_INTEGER;
  end;

  //Type definition for getting physical drive number
  TVolumeDiskExtents = record
    NumberOfDiskExtents: DWORD;
    Extents: array[0..1] of TDiskExtent;
  end;

  //Drive Length information
  GET_LENGTH_INFORMATION = record
    Length: integer;
  end;

  //info about serial number
  MEDIA_SERIAL_NUMBER_DATA= packed record
    SerialNumberLength: Cardinal;
    Result:Cardinal;
    AuthCommand:Cardinal;
    Reserved:Cardinal;
    SerialNumberData: array [0..MAX_PATH] of char;
   end;

   //This structure prevents media removal
  PREVENT_MEDIA_REMOVAL  = record
    PreventMediaRemoval : ByteBool;
  end;

implementation
end.
