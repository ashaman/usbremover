{
  Partial translation of Dbt.h
}

unit Dbt;

interface

const
  USBDevicePath = '\\?\USB#';
  USBStorageDevicePath = '\\?\USBSTOR#';
  VolumePath = '\\?\STORAGE#';
  
type

  PDEV_BROADCAST_HDR = ^TDEV_BROADCAST_HDR;
  _DEV_BROADCAST_HDR = record
    dbch_size: Cardinal;
    dbch_devicetype: Cardinal;
    dbch_reserved: Cardinal;
   end;
  TDEV_BROADCAST_HDR = _DEV_BROADCAST_HDR;

  PDEV_BROADCAST_DEVICEINTERFACE = ^ TDEV_BROADCAST_DEVICEINTERFACE;
  _DEV_BROADCAST_DEVICEINTERFACE = record
    dbcc_size: Cardinal;
    dbcc_devicetype:Cardinal;
    dbcc_reserved:Cardinal;
    dbcc_classguid:TGUID;
    dbcc_name:array[0..0] of char;
   end;
   TDEV_BROADCAST_DEVICEINTERFACE = _DEV_BROADCAST_DEVICEINTERFACE;

  PDEV_BROADCAST_HANDLE = ^TDEV_BROADCAST_HANDLE;
  _DEV_BROADCAST_HANDLE = record
    dbch_size: Cardinal;
    dbch_devicetype: Cardinal;
    dbch_reserved: Cardinal;
    dbch_handle: THandle;
    dbch_hdevnotify: Pointer;
    dbch_eventguid: TGUID;
    dbch_nameoffset:Integer;
    dbch_data:array[0..0] of BYTE;
   end;
  TDEV_BROADCAST_HANDLE = _DEV_BROADCAST_HANDLE;

  PDEV_BROADCAST_OEM = ^TDEV_BROADCAST_OEM;
  _DEV_BROADCAST_OEM = record
    dbco_size,
    dbco_devicetype,
    dbco_reserved,
    dbco_identifier,
    dbco_suppfunc:Cardinal;
   end;
  TDEV_BROADCAST_OEM =_DEV_BROADCAST_OEM;

  PDEV_BROADCAST_PORT = ^TDEV_BROADCAST_PORT;
  _DEV_BROADCAST_PORT = record
    dbcp_size,
    dbcp_devicetype,
    dbcp_reserved: Cardinal;
    dbcp_name:array[0..0] of char;
   end;
  TDEV_BROADCAST_PORT = _DEV_BROADCAST_PORT;

  PDEV_BROADCAST_VOLUME = ^TDEV_BROADCAST_VOLUME;
  _DEV_BROADCAST_VOLUME = record
    dbcv_size,
    dbcv_devicetype,
    dbcv_reserved,
    dbcv_unitmask:Cardinal;
    dbcv_flags:WORD;
   end;
  TDEV_BROADCAST_VOLUME = _DEV_BROADCAST_VOLUME;

const
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

  DBT_DEVTYP_OEM       = 0;
  DBT_DEVTYP_DEVNODE	 = 1;
  DBT_DEVTYP_VOLUME    = 2;
  DBT_DEVTYP_PORT	     = 3;
  DBT_DEVTYP_NET	     = 4;
  DBT_DEVTYP_DEVICEINTERFACE = 5;
  DBT_DEVTYP_HANDLE    = 6;

  DBTF_MEDIA = $0001;
  DBTF_NET   = $0002;

  DEVICE_NOTIFY_ALL_INTERFACE_CLASSES =  $0004;

implementation

end.
