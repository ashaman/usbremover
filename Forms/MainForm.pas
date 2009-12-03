unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, USBManager, StdCtrls, Device, ComCtrls, ExtCtrls;

type
  TMainFrm = class(TForm)
    ComboBox1: TComboBox;
    TreeView1: TTreeView;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Button1: TButton;
    procedure ComboBox1Change(Sender: TObject);
    procedure FillDrives(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AddInfo(Device: TDevice; Parent: TTreeNode);
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

uses
  Volume, Drive;

{$R *.dfm}

procedure TMainFrm.AddInfo(Device: TDevice; Parent: TTreeNode);
var
  i: integer;
  treeNode: TTreeNode;
begin
  treeNode := TreeView1.Items.AddChild(Parent, Device.Description + ' ' + Device.FriendlyName);
  for i := 0 to Device.Count-1 do
  begin
    AddInfo(Device.Children[i], treeNode);
  end;
end;

procedure TMainFrm.FillDrives;
var
  i: integer;
  device: TDevice;
  treeNode: TTreeNode;
  rm: TUSBManager;
begin
  rm := TUSBManager.GetManager;
  ComboBox1.Items.Clear;
  TreeView1.Items.Clear;
  treeNode := TreeView1.Items.GetFirstNode;
  for i := 0 to rm.GetDeviceCount-1 do
  begin
    device := rm.GetDeviceInfo(i);
    ComboBox1.Items.Add(device.Description);
    AddInfo(device, treeNode);
    ComboBox1.ItemIndex := 0;
  end;
end;

procedure TMainFrm.ComboBox1Change(Sender: TObject);
begin
  TUSBManager.GetManager.RemoveDrive(ComboBox1.ItemIndex);
end;

procedure TMainFrm.Button1Click(Sender: TObject);
begin
  FillDrives(self);
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  TUSBManager.GetManager.AddHandler(self.FillDrives);
end;

end.
