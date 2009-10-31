program USBRemover;

uses
  Forms,
  MainForm in 'MainForm.pas' {MainFrm},
  IDeviceRemover in 'IDeviceRemover.pas',
  USB in 'USB.pas',
  DeviceDeclarations in 'DeviceDeclarations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
