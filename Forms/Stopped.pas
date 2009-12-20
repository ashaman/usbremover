unit Stopped;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Device;

type
  TProcessesForm = class(TForm)
    Panel1: TPanel;
    treeViewProcesses: TTreeView;
    Panel2: TPanel;
    btnRescan: TButton;
    Label1: TLabel;
    btnCancel: TButton;
    searchProgressBar: TProgressBar;
    Label2: TLabel;
    procedure btnTryAgainClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnRescanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    fBlockedDevice: TDevice;
    function GetProgressCallback: TNotifyEvent;
    procedure RemovalSucceeded(Sender: TObject);
    procedure ReportProgress(Sender: TObject);
    procedure Refresh(Sender: TObject);
  public
    property BlockedDevice: TDevice read fBlockedDevice write fBlockedDevice;
    property ProgressCallback: TNotifyEvent read GetProgressCallback;
  end;

var
  ProcessesForm: TProcessesForm;

implementation

uses
  USBManager, Process, ProcessManager;

{$R *.dfm}

{IMPLEMENTED}

procedure TProcessesForm.Refresh(Sender: TObject);
var
  lockers: TList; //blockers
  i,j: integer; //loop indexes
  process: TProcess; //temp
  treeNode: TTreeNode; //temp
begin
  treeViewProcesses.Items.Clear;
  lockers := TProcessManager.GetInstance.GetLockers(fBlockedDevice.MountPoints,
    self.ProgressCallback);
  for i := 0 to lockers.Count-1 do
  begin
    process := TProcess(lockers.Items[i]);
    treeNode := treeViewProcesses.Items.AddChildFirst(nil, process.Name);
    for j := 0 to process.OpenedFiles.Count-1 do
    begin
      treeViewProcesses.Items.AddChild(treeNode, process.OpenedFiles.Strings[i]);
    end; //loop-j
  end; //loop-i
end;

procedure TProcessesForm.ReportProgress(Sender: TObject);
begin
  searchProgressBar.Position := Integer(Sender);
end;

function TProcessesForm.GetProgressCallback: TNotifyEvent;
begin
  Result := self.ReportProgress;
end;

procedure TProcessesForm.RemovalSucceeded(Sender: TObject);
begin
  self.Hide;
end;

{IMPLEMENTED}

procedure TProcessesForm.btnTryAgainClick(Sender: TObject);
begin
  TUSBManager.GetManager.RemoveDrive(fBlockedDevice);
end;

procedure TProcessesForm.FormCreate(Sender: TObject);
begin
  TUSBManager.GetManager.RemovalSucceeded.Attach(self.RemovalSucceeded);
end;

procedure TProcessesForm.FormActivate(Sender: TObject);
begin
  Refresh(Sender);
end;

procedure TProcessesForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TProcessesForm.btnRescanClick(Sender: TObject);
begin
  FormActivate(nil);
end;

procedure TProcessesForm.FormShow(Sender: TObject);
begin
  TUSBManager.GetManager.RemovalFailed.Attach(self.Refresh);
end;

procedure TProcessesForm.FormHide(Sender: TObject);
begin
  TUSBManager.GetManager.RemovalFailed.Detach(self.Refresh);
end;

end.
