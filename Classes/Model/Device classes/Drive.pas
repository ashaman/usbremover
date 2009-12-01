{
  Disk drive class.
  Written by J.L.Blackrow.
}

unit Drive;

interface

uses
  Device;

type
  //Disk device class
  TDiskDrive = class(TDevice)
  public
    constructor Create(Path: string);
    destructor Destroy; override;
  end;

implementation

uses
  WMI, SysUtils, Windows, WinIOCtl, DeviceException;

{CONSTRUCTOR}
constructor TDiskDrive.Create(Path: string);
begin
  inherited Create(GUID_DEVINTERFACE_DISK, Path);
  fBusType := GetBusType(Path);
  fFriendlyName := PChar(GetDeviceProperty(fDeviceInfoData, SPDRP_FRIENDLYNAME,
    fDeviceInfoSet, vkString));
end;

{DESTRUCTOR}
destructor TDiskDrive.Destroy;
begin
  inherited Destroy;
end;

end.

