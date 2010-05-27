unit DeviceTests;

interface

uses
  Device,
  TestFrameWork,
  WMI;

type
  TDeviceCracked = class(TDevice);

  TDeviceTests = class(TTestCase)
  private
    fDevice: TDevice;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published
    // Test methods
    procedure TestGetChild;
    procedure TestGetInstanceHandle;
    procedure TestGetDeviceInformation;
    procedure TestGetDeviceInformationSet;
    procedure TestGetDeviceProperty;
  end;


implementation

uses SysUtils, Windows;

procedure TDeviceTests.SetUp;
begin
  fDevice := TDevice.Create(GUID_DEVCLASS_DISKDRIVE,'');
end;

procedure TDeviceTests.TearDown;
begin
  fDevice.Destroy;
end;

procedure TDeviceTests.TestGetChild;
begin
  try
    fDevice.Children[-1];
  except
    on E: Exception do
      Check(E is EAccessViolation);
  end;
end;

procedure TDeviceTests.TestGetInstanceHandle;
begin
  fDevice.InstanceHandle;
end;

procedure TDeviceTests.TestGetDeviceInformation;
begin
end;

procedure TDeviceTests.TestGetDeviceInformationSet;
begin
  Check(TDeviceCracked.GetDeviceInformationSet(GUID_DEVCLASS_DISKDRIVE) <>
    INVALID_HANDLE_VALUE);
end;

procedure TDeviceTests.TestGetDeviceProperty;
begin

end;


initialization

  TestFramework.RegisterTest('DeviceTests Suite',
    TDeviceTests.Suite);

end.
 