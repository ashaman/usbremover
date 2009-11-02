{
  Device data class
}
unit Device;

interface

type
  TDevice = class(TObject)
  private
    fHandle: THandle;
    fName: PChar;
    fVolumeLabel: PChar;
    fManufacturer: PChar;
    function GetHandle: THandle;
    function GetName: PChar;
    function GetVolumeLabel: PChar;
    function GetManufacturer: PChar;
  public
    constructor Create(path: PChar);
    destructor Destroy; override;
    property Handle: THandle read GetHandle;
    property Name: PChar read GetName;
    property VolumeLabel: PChar read GetVolumeLabel;
    property Manufacturer: PChar read GetManufacturer;
  end;

{==============================================================================}
implementation

constructor TDevice.Create(path: PChar);
begin
  inherited Create;
  fVolumeLabel := path;
end;

destructor TDevice.Destroy;
begin
  inherited Destroy;
end;

{
  Getters and setters
}
function TDevice.GetHandle: THandle;
begin
  Result := fHandle;
end;

function TDevice.GetName: PChar;
begin
  Result := fName;
end;

function TDevice.GetVolumeLabel;
begin
  Result := fVolumeLabel;
end;

function TDevice.GetManufacturer: PChar;
begin
  Result := fManufacturer;
end;

end.
