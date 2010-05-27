// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program USBRemoverTests;

uses
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  Device in '..\Classes\Model\Device classes\Device.pas',
  Volume in '..\Classes\Model\Device classes\Volume.pas',
  Drive in '..\Classes\Model\Device classes\Drive.pas',
  WinIOCtl in '..\Classes\Model\API\WinIOCtl.pas',
  WMI in '..\Classes\Model\API\WMI.pas',
  DeviceException in '..\Classes\Model\Exceptions\DeviceException.pas',
  DeviceManager in '..\Classes\Model\Device managers\DeviceManager.pas',
  ShellObjExtended in '..\Classes\Model\API\ShellObjExtended.pas',
  USBManagerTests in '..\Classes\Model\Device managers\dunit\USBManagerTests.pas',
  USBManager in '..\Classes\Model\Device managers\USBManager.pas',
  USBDevice in '..\Classes\Model\Device classes\USBDevice.pas';

{$R *.RES}

begin
  Application.Initialize;

{$IFDEF LINUX}
  QGUITestRunner.RunRegisteredTests;
{$ELSE}
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
{$ENDIF}

end.

 