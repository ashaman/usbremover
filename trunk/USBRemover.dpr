{
  USB Safely Remover
  Application developed by CS-VSU students
  Freeware
}

program USBRemover;

uses
  Forms,
  MainForm in '.\Forms\MainForm.pas' {MainFrm - Main application window},
  USBManager in '.\Classes\Model\Device managers\USBManager.pas',
  Device in '.\Classes\Model\Device classes\Device.pas',
  DeviceManager in '.\Classes\Model\Device managers\DeviceManager.pas',
  WinIOCtl in '.\Classes\Model\API\WinIOCtl.pas',
  DeviceException in '.\Classes\Model\Exceptions\DeviceException.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
