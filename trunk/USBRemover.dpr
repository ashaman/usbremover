{
  USB Safely Remover
  Application developed by CS-VSU student Yaroslav Vorontsov
    a.k.a. J.L. Blackrow
  This program is freeware
}

{
  TODO: forms, controllers, process killing and adequate info output
}

{
  TODO: implement as system service
}

program USBRemover;

uses
  Forms,
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
  BroadcastEvent in 'Classes\Controller\BroadcastEvent.pas',
  ProcessManager in 'Classes\Model\Process manager\ProcessManager.pas',
  NTDLL in 'Classes\Model\API\NTdll.pas',
  Process in 'Classes\Model\Process\Process.pas',
  MainFormController in 'Classes\Controller\MainFormController.pas',
  Main in 'Forms\Main.pas' {MainAppForm - main application form},
  Stopped in 'Forms\Stopped.pas' {ProcessesForm};

{$R *.res}
begin
  Application.Initialize;
  Application.ShowMainForm := false;
  Application.Title := 'USBRemover';
  Application.CreateForm(TMainAppForm, MainAppForm);
  Application.CreateForm(TProcessesForm, ProcessesForm);
  Application.Run;
end.
