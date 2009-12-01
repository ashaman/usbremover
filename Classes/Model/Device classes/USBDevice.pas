{
  USB Device class.
  Developed by J.L.Blackrow
}
unit USBDevice;

interface

uses
  Device;

type
  TUSBDevice = class(TDevice)
  public
    constructor Create(InstanceHandle: Cardinal);
    destructor Destroy; override;
  end;

implementation

uses
  WMI;

{CONSTRUCTOR}
constructor TUSBDevice.Create(InstanceHandle: Cardinal);
begin
  inherited Create(GUID_DEVCLASS_USB, InstanceHandle);
end;

{DESTRUCTOR}
destructor TUSBDevice.Destroy;
begin
  inherited Destroy;
end;

end.
