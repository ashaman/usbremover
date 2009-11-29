{
  Class which describes device volume.
}
{
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
    function GetVolumeSize(Path: string): Int64; //gets volume size
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    class function GetVolumeMountPoints(Path: String): TStringList; //gets volume mount points
    //properties
    property VolumeLabel: string read fVolumeLabel;
    property VolumeID: Cardinal read fVolumeID;
    property VolumeRootDirectories: TStringList read fVolumeRootDirectories;
    property VolumeSize: Int64 read fVolumeSize;
    property VolumeFileSystemType: string read fVolumeFileSystemType;
  end;

implementation

uses
  WinIOCtl, Windows, DeviceException, SysUtils, WMI, ShellObjExtended;

//This function gets volume size 
function TVolume.GetVolumeSize(Path: string): Int64;
var
  bytesReturned: Cardinal; //bytes returned by the function
  lengthInfo: GET_LENGTH_INFORMATION; //length information
  handle: THandle; //file handle
begin
  Result := 0;
  handle := CreateFile(PChar(Path),GENERIC_READ, FILE_SHARE_READ or
    FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if handle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then - failed to open file
  else begin
    try
      //getting volume length
      if not DeviceIoControl(handle,IOCTL_DISK_GET_LENGTH_INFO,
        nil, 0, @lengthInfo, sizeof(lengthInfo), bytesReturned, nil)
      then begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError));
      end //then - getting failed
      else begin
        Result := lengthInfo.Length;
      end; //else - got length
    finally
      CloseHandle(handle);
    end;
  end; //else - file opened
end; //GetVolumeSize

//Gets volume info through WinAPI function GetVolumeInformation
procedure TVolume.GetVolumeInfo;
var
  VolumeNameBuf: TCharArray; //buffer for volume name
  FileSystemNameBuf: TCharArray; //buffer for file system name
  Success: LongBool; //indicates if a function call was successful
  ReturnedBytes: Cardinal; //buffer for returned bytes
  FSFlags: Cardinal; //file system flags set for device
begin
  //Getting volume information
  Success := GetVolumeInformation(PChar(fVolumeRootDirectories.Strings[0]),
    VolumeNameBuf, sizeof(VolumeNameBuf), @fVolumeID, ReturnedBytes, FSFlags,
    FileSystemNameBuf, sizeof(FileSystemNameBuf));
  if not Success
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //cannot get volume info
  else begin
    //setting volume label, FS type and size
    fVolumeLabel := String(VolumeNameBuf);
    fVolumeFileSystemType := String(FileSystemNameBuf);
  end;
end;

//Gets volume mount points using SetupAPI
class function TVolume.GetVolumeMountPoints(Path: String): TStringList;
var
  driveMountPoint: TCharArray; //buffer for the volume mount point name
  numPoints: Cardinal; //count of chars in mount points array
  pDriveMountPoint: PChar; //PChar buffer
begin
  //creating string list
  Result := TStringList.Create;
  numPoints := 0;
  //Getting volume mount points
  if GetVolumePathNamesForVolumeNameA(PChar(Path),
    PChar(@driveMountPoint), sizeof(driveMountPoint), numPoints)
  then begin
    pDriveMountPoint := PChar(@driveMountPoint);
    while (pDriveMountPoint[0] <> #0) do
    begin
      Result.Add(pDriveMountPoint);
      Inc(pDriveMountPoint, Length(pDriveMountPoint)+1); //+1 because of the #0
    end;
  end //then
  else begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end; //else
end; //GetVolumeMountPoints

//Constructor
constructor TVolume.Create(Path: string);
begin
  inherited Create(GUID_DEVINTERFACE_VOLUME, Path);
  fVolumeRootDirectories := GetVolumeMountPoints(Path);
  fVolumeSize := GetVolumeSize(fPath);
  GetVolumeInfo;
end;

//Destructor
destructor TVolume.Destroy;
begin
  if Assigned(fVolumeRootDirectories)
  then begin
    fVolumeRootDirectories.Free;
  end;
  inherited Destroy;
end;

end.

