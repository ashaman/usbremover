{
  USB Safely Remover
  Application developed by CS-VSU students Yaroslav Vorontsov and Yury Kuznetsov
  Freeware
}

program USBRemover;

uses
  Forms,
  MainForm in 'Forms\MainForm.pas' {MainFrm - Main application window},
  USBManager in 'Classes\Model\Device managers\USBManager.pas',
  Device in 'Classes\Model\Device classes\Device.pas',
  DeviceManager in 'Classes\Model\Device managers\DeviceManager.pas',
  WinIOCtl in 'Classes\Model\API\WinIOCtl.pas',
  DeviceException in 'Classes\Model\Exceptions\DeviceException.pas',
  WMI in 'Classes\Model\API\WMI.pas',
  Volume in 'Classes\Model\Device classes\Volume.pas',
  Win32_DiskDrive in 'Classes\Model\Device classes\Win32_DiskDrive.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
