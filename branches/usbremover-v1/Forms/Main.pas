{
  UI PART.
  DO NOT MODIFY.
  MADE BY J.L.Blackrow.
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CoolTrayIcon, Menus, ComCtrls, ToolWin, ImgList;

type
  TMainForm = class(TForm)
    MainPopupMenu: TPopupMenu;
    Show1: TMenuItem;
    Hide1: TMenuItem;
    About: TMenuItem;
    Exit1: TMenuItem;
    MainMenu: TMainMenu;
    Command1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    Index1: TMenuItem;
    About1: TMenuItem;
    Refresh1: TMenuItem;
    Settings1: TMenuItem;
    Showtoolbar1: TMenuItem;
    Stop1: TMenuItem;
    Properties1: TMenuItem;
    Exit3: TMenuItem;
    ToolBarMain: TToolBar;
    TreeViewDevices: TTreeView;
    StartConsole1: TMenuItem;
    PopupMenuDevices: TPopupMenu;
    ImageList: TImageList;
    ToolButtonStop: TToolButton;
    ToolButtonProperties: TToolButton;
    ToolButtonSettings: TToolButton;
    ToolButtonConsole: TToolButton;
    ToolButtonRefresh: TToolButton;
    CoolTrayIcon: TCoolTrayIcon;
    TreePopupMenu: TPopupMenu;
    Stop2: TMenuItem;
    Properties2: TMenuItem;
    procedure Show1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Showtoolbar1Click(Sender: TObject);
    procedure ToolButtonRefreshClick(Sender: TObject);
    procedure CoolTrayIconMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolButtonSettingsClick(Sender: TObject);
    procedure ToolButtonStopClick(Sender: TObject);
    procedure ToolButtonPropertiesClick(Sender: TObject);
    procedure ToolButtonConsoleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DevicesMenuItemClick(Sender: TObject);
    procedure TreeViewDevicesClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  USBManager, MainFormController;

var
  controller: TMainFormController;

//This procedure handles context menu clicks
procedure TMainForm.DevicesMenuItemClick(Sender: TObject);
var
  index: integer; //needed drive index
begin
  index := PopupMenuDevices.Items.IndexOf(Sender as TMenuItem);
  controller.RemoveDrive(index);
end; //DevicesMenuItemClick

//Shows/Hides main form
procedure TMainForm.Show1Click(Sender: TObject);
begin
  if not MainForm.Showing //form is hidden
  then begin
    CoolTrayIcon.ShowMainForm;
    Show1.Caption := 'Hide main window';
  end //then - tag is 0 - form is hidden
  else begin
    CoolTrayIcon.HideMainForm;
    Show1.Caption := 'Show main window';
  end; //else - tag is 1 - form is shown
end; //Show1Click

//Terminates application
procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end; //ExitClick

//This procedure changes the toolbar visibility
procedure TMainForm.Showtoolbar1Click(Sender: TObject);
begin
  Showtoolbar1.Checked := not Showtoolbar1.Checked;
  ToolBarMain.Visible := Showtoolbar1.Checked;
end; //Showtoolbar1Click

//Refreshes current list of devices
procedure TMainForm.ToolButtonRefreshClick(Sender: TObject);
begin
  controller.Refresh;
end; //ToolButtonRefreshClick

//This procedure changes the menus in the tray icon
procedure TMainForm.CoolTrayIconMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //ShowMessage(Sender.ClassName);
  if Button = mbLeft
  then begin
    CoolTrayIcon.LeftPopup := true;
    CoolTrayIcon.PopupMenu := PopupMenuDevices;
  end //click by the left button
  else if Button = mbRight
  then begin
    CoolTrayIcon.LeftPopup := false;
    CoolTrayIcon.PopupMenu := MainPopupMenu;
  end; //click by the rigth button
end; //CoolTrayIconMouseDown

procedure TMainForm.ToolButtonSettingsClick(Sender: TObject);
begin
  //NOT YET...
end;

//This procedure handles "Stop" clicks
procedure TMainForm.ToolButtonStopClick(Sender: TObject);
begin
  controller.RemoveDrive(TreeViewDevices.Selected.Index);
end; //StopClick

procedure TMainForm.ToolButtonPropertiesClick(Sender: TObject);
begin
  //NOT YET...
end;

procedure TMainForm.ToolButtonConsoleClick(Sender: TObject);
begin
  //NOT YET...
end;

//When the form is created, this method is invoked
procedure TMainForm.FormCreate(Sender: TObject);
begin
  controller := TMainFormController.Create(self);
end; //FormCreate

procedure TMainForm.TreeViewDevicesClick(Sender: TObject);
begin
  //checking if there are any available elements
  if Assigned(TreeViewDevices.Selected)
  then begin
    Properties1.Enabled := true;
    ToolButtonProperties.Enabled := true;
    TreeViewDevices.PopupMenu := TreePopupMenu;
    //checking if it is a root element
    if not Assigned(TreeViewDevices.Selected.Parent)
    then begin
      Stop1.Enabled := true;
      Stop2.Enabled := true;
      ToolButtonStop.Enabled := true;
      Exit;
    end; //then - root element chosen
  end  //then - some elements present
  else begin
    TreeViewDevices.PopupMenu := nil;
    Properties1.Enabled := false;
    ToolButtonProperties.Enabled := false;
  end; //else - no elements
  //else - not a root element chosen
  Stop1.Enabled := false;
  Stop2.Enabled := false;
  ToolButtonStop.Enabled := false;
end;

//Changes the caption of menu item
procedure TMainForm.FormHide(Sender: TObject);
begin
  Show1.Caption := 'Show main window';
end; //FormHide

end.
