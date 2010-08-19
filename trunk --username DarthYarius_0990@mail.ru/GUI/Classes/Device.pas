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

        function GetChildCount: integer; //child count
        function GetChild(index: integer): TDevice; //gets a child device
    public
        constructor Create(deviceInfo: DEVINFO); //constructor
        destructor Destroy; override; //destructor

        procedure AddChild(var device: TDevice); //adds a child
        procedure AddMountPoint(point: TWideCharArray); //adds a device mount point
        property ChildCount: integer read GetChildCount; //child count
        property Children[index: integer]: TDevice read GetChild; //children
        property Index: DEVINDEX read fIndex; //device index
    end;

implementation

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
end; //etChildCount

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
end;

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
    fMountPoints.Capacity := deviceInfo.dwMountPtsCount;
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

