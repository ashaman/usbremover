{
  Disk drive class.
  Written by J.L.Blackrow.
}

{
  TODO: get bus number through DeviceIoControl      //maybe?
}

unit Drive;

interface

uses
  Device;

type
  TDiskDrive = class(TDevice)
  public
    constructor Create(Path: string);
    destructor Destroy; override;
  end;

implementation

uses
  WMI, SysUtils;

{CONSTRUCTOR}
constructor TDiskDrive.Create;
begin
  inherited Create(GUID_DEVCLASS_DISKDRIVE, Path);
  fBusType := TBusType(GetDeviceProperty(fDeviceNumber.DeviceNumber,
    fDeviceInfoData, SPDRP_BUSNUMBER, fDeviceInfoSet, vkNumber)^);
  fFriendlyName := String(GetDeviceProperty(fDeviceNumber.DeviceNumber,
    fDeviceInfoData, SPDRP_FRIENDLYNAME, fDeviceInfoSet, vkString));
end;

{DESTRUCTOR}
destructor TDiskDrive.Destroy;
begin
  inherited Destroy;
end;

end.
