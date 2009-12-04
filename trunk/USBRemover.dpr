{
  USB Safely Remover
  Application developed by CS-VSU student Yaroslav Vorontsov
    a.k.a J.L. Blackrow
  Freeware
}

program USBRemover;

uses
  Forms,
  MainForm in 'Forms\MainForm.pas' {MainFrm - Main application window},
  USBManager in 'Classes\Model\Device managers\USBManager.pas',
  DeviceManager in 'Classes\Model\Device managers\DeviceManager.pas',
  WinIOCtl in 'Classes\Model\API\WinIOCtl.pas',
  DeviceException in 'Classes\Model\Exceptions\DeviceException.pas',
  WMI in 'Classes\Model\API\WMI.pas',
  Volume in 'Classes\Model\Device classes\Volume.pas',
  ShellObjExtended in 'Classes\Model\API\ShellObjExtended.pas',
  Device in 'Classes\Model\Device classes\Device.pas',
  Drive in 'Classes\Model\Device classes\Drive.pas',
  USBDevice in 'Classes\Model\Device classes\USBDevice.pas',
  Dbt in 'Classes\Model\API\Dbt.pas',
  BroadcastEvent in 'Classes\Controller\BroadcastEvent.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
