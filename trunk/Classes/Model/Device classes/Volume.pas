{
  Class which describes device volume.

  TODO: add class function that allows to get physical location of the volume
  through IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS
  //Retrieves the physical location of the specified volume on one or more disks. 
}

unit Volume;

interface

uses
  Device, Classes;

type
  TVolume = class(TDevice)
  private
    fVolumeLabel: string; //volume label
    fVolumeID: Cardinal; //volume numeric ID
    fVolumeRootDirectories: TStringList; //volume root directory
    fVolumeSize: Int64; //volume size
    fVolumeFileSystemType: string; //volume file system type
    procedure GetVolumeInfo; //gets volume information
    //getters
  public
    constructor Create(Path: string);
    destructor Destroy; override;
    //properties
    property VolumeLabel: string read fVolumeLabel;
    property VolumeID: Cardinal read fVolumeID;
    property VolumeRootDirectories: TStringList read fVolumeRootDirectories;
    property VolumeSize: Int64 read fVolumeSize;
    property VolumeFileSystemType: string read fVolumeFileSystemType;
  end;

implementation

uses
  WinIOCtl, Windows, DeviceException, SysUtils, WMI;

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
  Success := GetVolumeInformation(PChar(fVolumeRootDirectories.Strings[0]),
    VolumeNameBuf, MAX_PATH, @fVolumeID, ReturnedBytes, FSFlags,
    FileSystemNameBuf, MAX_PATH);
  if not Success
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //cannot get volume info
  else begin
    //setting volume label and FS type
    fVolumeLabel := String(VolumeNameBuf);
    fVolumeFileSystemType := String(@FileSystemNameBuf);
    //TODO get volume size... How?!?!
  end;
end;

//Constructor
constructor TVolume.Create(Path: string);
begin
  inherited Create(GUID_DEVINTERFACE_VOLUME, Path);
  GetVolumeInfo;
end;

//Destructor
destructor TVolume.Destroy;
begin
  inherited Destroy;
end;

end.
