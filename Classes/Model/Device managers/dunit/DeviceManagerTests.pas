unit DeviceManagerTests;

interface

uses
  DeviceManager,
  TestFrameWork;

type
  TDeviceManagerTests = class(TTestCase)
  private

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestBuildAll;
    procedure TestRemoveDrive;
    procedure TestForcedRemoveDrive;
    procedure TestGetBlockedFiles;
    procedure TestGetDeviceInfo;
    procedure TestGetDeviceCount;
  end;

implementation

var
  mgr: TDeviceManager;

procedure TDeviceManagerTests.SetUp;
begin
  mgr := TDeviceManager.Create;
end;

procedure TDeviceManagerTests.TearDown;
begin
end;

procedure TDeviceManagerTests.TestBuildAll;
begin
end;

procedure TDeviceManagerTests.TestRemoveDrive;
begin
end;

procedure TDeviceManagerTests.TestForcedRemoveDrive;
begin
end;

procedure TDeviceManagerTests.TestGetBlockedFiles;
begin
end;

procedure TDeviceManagerTests.TestGetDeviceInfo;
begin
end;

procedure TDeviceManagerTests.TestGetDeviceCount;
begin
end;

initialization

  TestFramework.RegisterTest('DeviceManagerTests Suite',
    TDeviceManagerTests.Suite);

end.
 