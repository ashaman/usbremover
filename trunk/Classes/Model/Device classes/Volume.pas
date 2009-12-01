{
  Class which describes device volume.
  Developed by J.L.Blackrow
}
unit Volume;

interface

uses
  Device, Classes;

type
  TVolume = class(TDevice)
  private
    fDiskNumbers: TList; //disk numbers where volume is mounted
    fVolumeLabel: string; //volume label
    fVolumeID: Cardinal; //volume numeric ID
    fVolumeRootDirectories: TStringList; //volume root directory
    fVolumeSize: Int64; //volume size
    fVolumeFileSystemType: string; //volume file system type
    function GetDiskCount: Cardinal; //disk count
    function GetDiskNumbers(Path: string): TList; //<Cardinal> - gets volume disk numbers
    procedure GetVolumeInfo; //gets volume information
    function GetVolumeSize(Path: string): Int64; //gets volume size
  public
    constructor Create(Path: string);
    destructor Destroy; override;
    procedure NotifySystem; override;
    class function GetVolumeMountPoints(Path: String): TStringList; //gets volume mount points
    //properties
    property VolumeDiskCount: Cardinal read GetDiskCount;
    property VolumeDiskNumbers: TList read fDiskNumbers;
    property VolumeLabel: string read fVolumeLabel;
    property VolumeID: Cardinal read fVolumeID;
    property VolumeRootDirectories: TStringList read fVolumeRootDirectories;
    property VolumeSize: Int64 read fVolumeSize;
    property VolumeFileSystemType: string read fVolumeFileSystemType;
  end;

implementation

uses
  WinIOCtl, Windows, DeviceException, SysUtils, WMI, ShellObjExtended, ShlObj;

//This function hets total disk count where this volume is mounted
function TVolume.GetDiskCount: Cardinal;
begin
  Result := fDiskNumbers.Count;
end;

//This function gets disk numbers where this volume is mounted
function TVolume.GetDiskNumbers(Path: string): TList;
var
  handle: THandle; //file handle
  dummy: Cardinal; //bytes returned by function
  diskExtents: TVolumeDiskExtents; //volume-disk extents
  i: integer; //loop index
begin
  Result := nil;
  handle := CreateFile(PChar(@Path[1]), GENERIC_READ, FILE_SHARE_READ or
    FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if handle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then - opening failed
  else begin
    try
      //getting volume extents
      if not DeviceIoControl(handle, IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
        nil, 0, @diskExtents, sizeof(diskExtents), dummy, nil)
      then begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError));
      end //then - error occured
      else begin
        Result := TList.Create;
        for i := 0 to diskExtents.NumberOfDiskExtents-1 do
        begin
          Result.Add(@diskExtents.Extents[i].DiskNumber);
        end; //adding disk numbers
      end; //else
    finally
      CloseHandle(handle);
    end; //finally
  end; //else - opened successfully
end; //GetDiskNumbers

//This function notifies the OS about the volume removal
procedure TVolume.NotifySystem;
var
  i: integer; //loop index
begin
  for i := 0 to fVolumeRootDirectories.Count-1 do
  begin
    SHChangeNotify(SHCNE_MEDIAREMOVED, SHCNF_PATH,
      @fVolumeRootDirectories.Strings[i][1], nil);
  end;
end;

//This function gets volume size
function TVolume.GetVolumeSize(Path: string): Int64;
var
  bytesReturned: Cardinal; //bytes returned by the function
  lengthInfo: GET_LENGTH_INFORMATION; //length information
  handle: THandle; //file handle
begin
  Result := 0;
  handle := CreateFile(PChar(@Path[1]),GENERIC_READ, FILE_SHARE_READ or
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
  Success := GetVolumeInformation(PChar(@fVolumeRootDirectories.Strings[0][1]),
    VolumeNameBuf, sizeof(VolumeNameBuf), @fVolumeID, ReturnedBytes, FSFlags,
    FileSystemNameBuf, sizeof(FileSystemNameBuf));
  if not Success
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //cannot get volume info
  else begin
    //setting volume label, FS type and size
    fVolumeLabel := String(VolumeNameBuf);
    fFriendlyName := fVolumeLabel;
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
  if GetVolumePathNamesForVolumeNameA(PChar(@Path[1]),
    PChar(@driveMountPoint), sizeof(driveMountPoint), numPoints)
  then begin
    pDriveMountPoint := PChar(@driveMountPoint[0]);
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
  fDiskNumbers := GetDiskNumbers(fPath);
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




