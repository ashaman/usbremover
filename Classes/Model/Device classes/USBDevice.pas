{
  USB Device class.
  Developed by J.L.Blackrow
}
unit USBDevice;

interface

uses
  Device, Classes;

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
  inherited Create(GUID_DEVINTERFACE_USB_DEVICE, InstanceHandle);
end;

{DESTRUCTOR}
destructor TUSBDevice.Destroy;
begin
  inherited Destroy;
end;

end.
