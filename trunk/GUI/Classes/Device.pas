{
    Device.pas
    Started: 18.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Defines device class
}
unit Device;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Communication;

type
    {
        Description:
            This class represents the device
    }
    TDevice = class(TObject)
    private
        fIndex: DEVINDEX; //device index information
        fParent: TDevice; //parent device
        fChildren: TList; //child devices
        fDescription: WideString; //description
        fName: WideString; //device name
        fMountPoints: TStrings; //device mount points

        function GetChild(index: integer): TDevice; //gets a child device
        function GetChildCount: integer; //child count
        function GetMountPoint(index: integer): string; //mpoint
        function GetMountPointCount: integer; //mpcount
    public
        constructor Create(deviceInfo: DEVINFO); //constructor
        destructor Destroy; override; //destructor

        procedure AddChild(var device: TDevice); //adds a child
        procedure AddMountPoint(point: TWideCharArray); //adds a device mount point
        property ChildCount: integer read GetChildCount; //child count
        property Children[index: integer]: TDevice read GetChild; //children
        property Description: WideString read fDescription; //description
        property Index: DEVINDEX read fIndex; //device index
        property MountPointCount: integer read GetMountPointCount; //mtps count
        property MountPoints[index: integer]: string read GetMountPoint; //mpts
        property Name: WideString read fName; //device name
    end;

implementation

{
    Purpose:
        Gets the mount point by index
    Parameters:
        index - mount point index
    Return value:
        String with the mount point
}
function TDevice.GetMountPoint(index: integer): string;
begin
    Result := Self.fMountPoints.Strings[index];
end; //GetMountPoint

{
    Purpose:
        Get the number of mount points
    Parameters:
        None
    Return value:
        Total count of mount points
}
function TDevice.GetMountPointCount: integer;
begin
    Result := Self.fMountPoints.Count;
end; //GetMountPointCount

{
    Purpose:
        Gets the number of children
    Parameters:
        None
    Return value:
        Count of children
}
function TDevice.GetChildCount: integer;
begin
    Result := fChildren.Count;
end; //GetChildCount

{
    Purpose:
        Adds a new mount point to the device
    Parameters:
        point - LPWSTR-string for mount point
    Return value:
        None
}
procedure TDevice.AddMountPoint(point: TWideCharArray);
begin
    fMountPoints.Add(Trim(point));
end; //AddMountPoint

{
    Purpose:
        Gets a child by its index. Used for indexer
    Parameters:
        index - child index
    Return value:
        Child device
}
function TDevice.GetChild(index: integer): TDevice;
begin
    Result := TDevice(fChildren.Items[index]);
end; //GetChild

{
    Purpose:
        Adds a new child
    Parameters:
        device - child device
    Return value:
        None
}
procedure TDevice.AddChild(var device: TDevice);
begin
    device.fParent := Self;
    fChildren.Insert(device.Index.dwDeviceNumber, device);
end; //AddChild

{
    Purpose:
        Constructor. Gets the device information
        from the DEVINFO structure
    Parameters:
        deviceInfo - device information
}
constructor TDevice.Create(deviceInfo: DEVINFO);
begin
    inherited Create;
    fIndex := deviceInfo.devIndex;
    fParent := nil;
    fChildren := TList.Create;
    fMountPoints := TStringList.Create;
    fMountPoints.Capacity := deviceInfo.MountPtsCount;
    fDescription := deviceInfo.description;
    fName := deviceInfo.name;
end; //Create

{
    Purpose:
        Destructor
}
destructor TDevice.Destroy;
var
    i: integer; //loop index
begin
    fParent := nil;
    for i := 0 to fChildren.Count-1 do
    begin
        TDevice(fChildren.Items[i]).Destroy;
    end;
    fChildren.Destroy;
    fMountPoints.Destroy;
    inherited Destroy;
end; //Destroy

end.

