unit USB;

interface
uses
  IDeviceRemover, Windows, DeviceDeclarations;
type
  TUSBManager = class(TDeviceManager)
  private
    procedure Listener(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RemoveDrive; override;
    function GetDeviceInfo: TDevice; override;
  end;

implementation

constructor TUSBManager.Create;
begin
  inherited Create;
end;

destructor TUSBManager.Destroy;
begin
  inherited Destroy;
end;

procedure TUSBManager.RemoveDrive;
begin
end;

function TUSBManager.GetDeviceInfo: TDevice;
begin
end;

procedure TUSBManager.Listener(Sender: TObject);
begin
end;

procedure test;
var
  mng: TUSBManager;
begin
  mng := TUSBManager.Create;
  mng.AddHandler(mng.Listener);
end;

end.
