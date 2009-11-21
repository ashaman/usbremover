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
  DeviceTests in 'DeviceTests.pas',
  Device in '..\Classes\Model\Device classes\Device.pas',
  WinIOCtl in '..\Classes\Model\API\WinIOCtl.pas',
  WMI in '..\Classes\Model\API\WMI.pas',
  DeviceException in '..\Classes\Model\Exceptions\DeviceException.pas';

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

 