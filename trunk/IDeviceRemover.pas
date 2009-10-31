unit IDeviceRemover;

interface
uses
  Windows, Classes, DeviceDeclarations;

type
  //Abstract class for device manager
  TDeviceManager = class(TObject)
  private
    eventHandlers: TList;
  protected
    devices: TList;
    constructor Create;
    procedure NotifyAll;
  public
    destructor Destroy; override;

    //These functions depend on device type
    procedure RemoveDrive; virtual; abstract;
    procedure ForcedRemoveDrive; virtual; abstract;
    function GetDeviceInfo: TDevice; virtual; abstract;

    //These funtions are common for all devices
    function GetBlockedFiles: TStrings;
    function GetBlockerID: HWND;

    //These functions add event handlers from listeners
    procedure AddHandler(Handler: TNotifyEvent);
    procedure RemoveHandler(Handler: TNotifyEvent);
  end;

implementation

constructor TDeviceManager.Create;
begin
  inherited Create;
  eventHandlers := TList.Create;
  devices := TList.Create;
end;

destructor TDeviceManager.Destroy;
begin
  eventHandlers.Destroy;
  devices.Destroy;
  inherited Destroy;
end;

function TDeviceManager.GetBlockerID;
begin

end;

function TDeviceManager.GetBlockedFiles: TStrings;
begin
end;

//Adds new listener to list
procedure TDeviceManager.AddHandler(Handler: TNotifyEvent);
begin
  if (eventHandlers.IndexOf(@Handler) = -1)
  then
    eventHandlers.Add(@Handler);
end;

//Removes event listener
procedure TDeviceManager.RemoveHandler(Handler: TNotifyEvent);
begin
  eventHandlers.Remove(@Handler);
end;

//Notifies all listeners about changes in this object
procedure TDeviceManager.NotifyAll;
var
  i, count: integer;
  event: TNotifyEvent;
begin
  count := eventHandlers.Count;
  for i := 0 to count-1 do
  begin
    event := TNotifyEvent(eventHandlers.Items[i]^);
    if Assigned(event)
    then
      event(Self);
  end;
end;

end.
