unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, USBManager, StdCtrls, Device, ComCtrls, ExtCtrls, SiteComp,
  CoolTrayIcon, Menus, JustOne;

type
  TMainFrm = class(TForm)
    ComboBox1: TComboBox;
    TreeView1: TTreeView;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    CoolTrayIcon1: TCoolTrayIcon;
    PopupMenu1: TPopupMenu;
    XJustOne1: TXJustOne;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FillDrives(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CoolTrayIcon1DblClick(Sender: TObject);
  private
    procedure PopupMenuClick(Sender: TObject);
    procedure AddInfo(Device: TDevice; Parent: TTreeNode);
  public
    procedure PBCallback(Sender: TObject);
  end;

var
  MainFrm: TMainFrm;

implementation

uses
  Volume, Drive, ProcessManager, Process;

var
  fullInfo: string;

{$R *.dfm}

procedure TMainFrm.PBCallback(Sender: TObject);
begin
  ProgressBar1.Position := Integer(Sender);
end;

procedure TMainFrm.AddInfo(Device: TDevice; Parent: TTreeNode);
var
  i: integer;
  treeNode: TTreeNode;
begin
  if fullInfo <> ''
  then begin
    fullInfo := fullInfo + '-(' + Device.Description + ' '
      + Device.FriendlyName + ')';
  end
  else begin
    fullInfo := '(' + Device.Description + ' '
      + Device.FriendlyName + ')';
  end;
  treeNode := TreeView1.Items.AddChild(Parent, Device.Description + ' ' + Device.FriendlyName);
  for i := 0 to Device.Count-1 do
  begin
    AddInfo(Device.Children[i], treeNode);
  end;
end;

procedure TMainFrm.PopupMenuClick(Sender: TObject);
var
  count: Integer;
begin
  count := TUSBManager.GetManager.GetDeviceCount;
  TUSBManager.GetManager.RemoveDrive(PopupMenu1.Items.IndexOf(Sender as TMenuItem));
  if count > TUSBManager.GetManager.GetDeviceCount
  then begin
    CoolTrayIcon1.ShowBalloonHint('Completed!','Device detached!', bitInfo, 15);
  end
  else begin
    CoolTrayIcon1.ShowBalloonHint('Failed!','Cannot remove device', bitError, 15);
  end;
end;

procedure TMainFrm.FillDrives(Sender: TObject);
var
  i: integer;
  device: TDevice;
  treeNode: TTreeNode;
  rm: TUSBManager;
  tmItem: TMenuItem;
begin
  rm := TUSBManager.GetManager;
  tmItem := TMenuItem.Create(nil);
  tmItem.OnClick := PopupMenuClick;
  ComboBox1.Items.Clear;
  TreeView1.Items.Clear;
  PopupMenu1.Items.Clear;
  treeNode := TreeView1.Items.GetFirstNode;
  for i := 0 to rm.GetDeviceCount-1 do
  begin
    device := rm.GetDeviceInfo(i);
    fullInfo := '';
    AddInfo(device, treeNode);
    tmItem.Caption := fullInfo;
    PopupMenu1.Items.Add(tmItem);
    ComboBox1.Items.Add(fullInfo);
    ComboBox1.ItemIndex := 0;
  end;
end;

procedure TMainFrm.ComboBox1Change(Sender: TObject);
var
  count: Integer;
  i,j: integer;
  tmp: TProcess;
  tnode: TTreeNode;
begin
  count := TUSBManager.GetManager.GetDeviceCount;
  TUSBManager.GetManager.RemoveDrive(ComboBox1.ItemIndex);
  if count > TUSBManager.GetManager.GetDeviceCount
  then begin
    CoolTrayIcon1.ShowBalloonHint('Completed!','Device detached!', bitInfo, 15);
  end
  else begin
    TreeView1.Items.Clear;
    count := TProcessManager.GetInstance.BlockerProcesses.Count;
    for i := 0 to count-1 do
    begin
      tmp := TProcess(TProcessManager.GetInstance.BlockerProcesses.Items[i]);
      tnode := TreeView1.Items.AddChild(nil, tmp.Name);
      for j := 0 to tmp.OpenedFiles.Count-1 do
      begin
        TreeView1.Items.AddChild(tnode, tmp.OpenedFiles.Strings[j]);
      end;
    end;
    CoolTrayIcon1.ShowBalloonHint('Failed!','Cannot remove device', bitError, 15);
  end;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  TUSBManager.GetManager.NotifyEvent.Attach(FillDrives);
end;

procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CoolTrayIcon1.IconVisible := false;
end;

procedure TMainFrm.CoolTrayIcon1DblClick(Sender: TObject);
begin
  Show;
end;

end.
