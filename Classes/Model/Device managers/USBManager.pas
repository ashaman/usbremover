{
  USB Device manager
  Made as singleton
  Inherited from TDeviceManager
}
unit USBManager;

interface
uses
  DeviceManager, Device, Messages, WinIOCtl, Windows, DeviceException;
type
  TUSBManager = class(TDeviceManager)
  private
    constructor Create;
  public
    destructor Destroy; override;
    procedure RemoveDrive; override;
    procedure ForcedRemoveDrive; override;
    procedure HandleMessages(var Msg: TMessage);
    class function GetManager: TUSBManager;
  end;

implementation
var
  Instance: TUSBManager;

//This constructor calls the parent one
//and passes as parameter removable drive type
constructor TUSBManager.Create;
begin
  inherited Create(DRIVE_REMOVABLE);
end;

//Overriden destructor
destructor TUSBManager.Destroy;
begin
  inherited Destroy;
end;

//This routine tries to remove USB Drive
procedure TUSBManager.RemoveDrive;
begin
  //DeviceIOControl
end;

//This routine
procedure TUSBManager.ForcedRemoveDrive;
begin
end;

class function TUSBManager.GetManager: TUSBManager;
begin
  if not Assigned(Instance)
  then begin
    Instance := TUSBManager.Create;
  end;
  Result := Instance;
end;

procedure TUSBManager.HandleMessages(var Msg: TMessage);
begin

end;


end.
