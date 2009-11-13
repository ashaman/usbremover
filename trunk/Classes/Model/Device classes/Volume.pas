{
  Class which describes device volume.
}

unit Volume;

interface

type
  TVolume = class(TObject)
  private
    fVolumeLabel: PChar; //volume label
    fVolumeID: Cardinal; //volume numeric ID
    fVolumeRootDirectory: PChar; //volume root directory
    fVolumeSize: Int64; //volume size
    fVolumeFileSystemType: PChar; //volume file system type
    procedure GetVolumeInfo; //gets volume information
    //getters
    function GetVolumeLabel: PChar;
    function GetVolumeID: Cardinal;
    function GetVolumeRootDirectory: PChar;
    function GetVolumeSize: Int64;
    function GetVolumeFileSystemType: PChar;
  public
    constructor Create(volumeRoot: PChar);
    destructor Destroy; override;
    //properties
    property VolumeLabel: PChar read GetVolumeLabel;
    property VolumeID: Cardinal read GetVolumeID;
    property VolumeRootDirectory: PChar read GetVolumeRootDirectory;
    property VolumeSize: Int64 read GetVolumeSize;
    property VolumeFileSystemType: PChar read GetVolumeFileSystemType;
  end;

implementation

uses
  WinIOCtl, Windows, DeviceException, SysUtils;

//Gets volume info through WinAPI function GetVolumeInformation
procedure TVolume.GetVolumeInfo;
var
  VolumeNameBuf: TCharArray; //buffer for volume name
  FileSystemNameBuf: TCharArray; //buffer for file system name
  Success: LongBool; //indicates if a function call was successful
  ReturnedBytes: Cardinal; //buffer for returned bytes
  FSFlags: Cardinal; //File system flags set for device
begin
  //Getting volume information
  Success := GetVolumeInformation(fVolumeRootDirectory, VolumeNameBuf, MAX_PATH,
    @fVolumeID, ReturnedBytes, FSFlags, FileSystemNameBuf, MAX_PATH);
  if not Success
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //cannot get volume info
  else begin
    //setting volume label and FS type
    fVolumeLabel := PChar(@VolumeNameBuf);
    fVolumeFileSystemType := PChar(@FileSystemNameBuf);
    fVolumeSize := DiskSize(ord(fVolumeRootDirectory[0])-ord(FLOPPY_DRIVE_1)+1);
  end;
end;

//Constructor
constructor TVolume.Create(volumeRoot: PChar);
begin
  inherited Create;
  fVolumeRootDirectory := volumeRoot;
  GetVolumeInfo;
end;

//Destructor
destructor TVolume.Destroy;
begin
  inherited Destroy;
end;

{
  GETTERS SECTION
}
function TVolume.GetVolumeLabel: PChar;
begin
  Result := fVolumeLabel;
end;

function TVolume.GetVolumeID: Cardinal;
begin
  Result := fVolumeID;
end;

function TVolume.GetVolumeRootDirectory: PChar;
begin
  Result := fVolumeRootDirectory;
end;

function TVolume.GetVolumeSize: Int64;
begin
  Result := fVolumeSize;
end;

function TVolume.GetVolumeFileSystemType: PChar;
begin
  Result := fVolumeFileSystemType;
end;

end.
