{
  WARNING!!!
  ALL THESE TESTS ARE HARDWARE-DEPENDENT!!!
  IT MAY NOT WORK PROPERLY!!!
}

unit USBManagerTests;

interface

uses
  USBManager,
  DeviceManager,
  TestFrameWork;

type
  TUSBManagerTests = class(TTestCase)
  private
    mgr: TUSBManager;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestRemoveDriveNil;
    procedure TestRemoveDriveNotBusy;
    procedure TestRemoveDriveIndexMinus;
    procedure TestForcedRemoveDrive;
    procedure TestGetManager;
    procedure TestGetDeviceInfoNegative;
    procedure TestGetDeviceInfoByName;
    procedure TestGetDeviceInfoOneFlash;
    procedure TestGetDeviceCountNoDevs;
    procedure TestGetDeviceCountOneFlashAttached;
    procedure TestGetDeviceCountTwoFlashAttached;
  end;

implementation

uses
  SysUtils, Classes, DeviceException, Device;

procedure TUSBManagerTests.SetUp;
begin
  mgr := TUSBManager.GetManager;
end;

procedure TUSBManagerTests.TearDown;
begin
  mgr := nil;
end;

procedure TUSBManagerTests.TestRemoveDriveNil;
begin
  try
    mgr.RemoveDrive(nil);
  except
    on EAccessViolation do
      Check(true, 'Null pointer');
  end;
end;

procedure TUSBManagerTests.TestRemoveDriveIndexMinus;
begin
  try
    mgr.RemoveDrive(-1);
  except
    on EDeviceException do
      Check(true, 'Negative index');
  end;
end;

procedure TUSBManagerTests.TestForcedRemoveDrive;
begin
end;

procedure TUSBManagerTests.TestGetManager;
begin
  Check(TUSBManager.GetManager <> nil, 'Check of singleton work');
end;

procedure TUSBManagerTests.TestGetDeviceInfoNegative;
begin
  try
    mgr.GetDeviceInfo(-1)
  except
    on EDeviceException do
      Check(true, 'Negative Index');
  end;
end;

procedure TUSBManagerTests.TestGetDeviceCountNoDevs;
begin
  Check(mgr.GetDeviceCount = 0, 'No flash drives');
end;

procedure TUSBManagerTests.TestGetDeviceCountOneFlashAttached;
begin
  Check(mgr.GetDeviceCount = 1, 'One flash attached');
end;

procedure TUSBManagerTests.TestGetDeviceCountTwoFlashAttached;
begin
  Check(mgr.GetDeviceCount = 2, 'Two flash attached');
end;

procedure TUSBManagerTests.TestGetDeviceInfoOneFlash;
var
  Device: TDevice;
begin
  try
    Device := mgr.GetDeviceInfo(0);
    Check(Device.Description='Запоминающее устройство для USB');
  except
    on EDeviceException do
      Check(true, 'No such flash drive');
  end;
end;

procedure TUSBManagerTests.TestGetDeviceInfoByName;
begin
  Check(false, 'Not yet implemented');
end;

procedure TUSBManagerTests.TestRemoveDriveNotBusy;
var
  old: integer;
begin
  try
    old := mgr.GetDeviceCount;
    mgr.RemoveDrive(1);
    Check(mgr.GetDeviceCount = old-1, 'Device detached / Is busy');
  except
    on EDeviceException do
      Check(true, 'No device with index 1 specified');
    on EDunitException do
      Check(true, 'Device is not ejected');
  end;
end;

initialization

  TestFramework.RegisterTest('USBManagerTests Suite',
    TUSBManagerTests.Suite);

end.
