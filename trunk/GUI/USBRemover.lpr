program USBRemover;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, MainForm, Settings, Connector, Device, Process, 
MainFormController, BlockedFilesForm;

{$IFDEF WINDOWS}{$R USBRemover.rc}{$ENDIF}

begin
  {$I USBRemover.lrs}
  Application.Initialize;
  //here we hide the main form
  Application.ShowMainForm := false;
  Application.CreateForm(TMainWnd, MainWnd);
  //and here we hide it from the taskbar
  MainWnd.ShowInTaskBar := stNever;
  Application.CreateForm(TBlockedFilesWnd, BlockedFilesWnd);
  BlockedFilesWnd.ShowInTaskBar := stNever;
  Application.Run;
end.

