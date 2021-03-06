{
    MainForm.pas
    Started: 22.05.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Main form declaration and implementation
}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, ComCtrls, LCLType, Messages,
  Communication, Settings; //application settings

type


  { TMainWnd }

  TMainWnd = class(TForm)
    DeviceImageList: TImageList;
    MenuImageList: TImageList;
    TVMIEject: TMenuItem;
    MIViewSettings: TMenuItem;
    TreeViewPopupMenu: TPopupMenu;
    ToolbarImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItemSeparator1: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MICommandEject: TMenuItem;
    TApmExit: TMenuItem;
    TApmSeparator: TMenuItem;
    TApmShowAboutDlg: TMenuItem;
    TApmShowSettings: TMenuItem;
    TApmShowMainWnd: TMenuItem;
    MIViewHideToolbar: TMenuItem;
    MIViewRefresh: TMenuItem;
    MIHelpAbout: TMenuItem;
    MIHelpIndex: TMenuItem;
    MICommand: TMenuItem;
    MIView: TMenuItem;
    MIHelp: TMenuItem;
    MICommandExit: TMenuItem;
    TBEject: TToolButton;
    TBSeparator2: TToolButton;
    TBExit: TToolButton;
    TBSeparator1: TToolButton;
    TBRefresh: TToolButton;
    TBHelp: TToolButton;
    TBAbout: TToolButton;
    TrayAppPopupMenu: TPopupMenu;
    TrayDisksPopupMenu: TPopupMenu;
    ToolBar: TToolBar;
    TrayIcon: TTrayIcon;
    DeviceTreeView: TTreeView;
    procedure DeviceTreeViewSelectionChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MICommandEjectClick(Sender: TObject);
    procedure MICommandExitClick(Sender: TObject);
    procedure MIHelpAboutClick(Sender: TObject);
    procedure MIHelpIndexClick(Sender: TObject);
    procedure MIViewHideToolbarClick(Sender: TObject);
    procedure MIViewRefreshClick(Sender: TObject);
    procedure TApmExitClick(Sender: TObject);
    procedure TApmShowMainWndClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure TrayIconMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fSupressTrayMenus: boolean; //supress or not

    procedure HandleUSRMessage(var Message: TMessage); message WM_USBREMOVER;
  public
    { public declarations }
  end; 

var
  MainWnd: TMainWnd;

{==============================================================================}
implementation

uses
    MainFormController; //main form controller

var
    controller:  TMainFormController; //controller instance

{ TMainWnd }

//Window message handler
procedure TMainWnd.HandleUSRMessage(var Message: TMessage);
begin
    controller.ResolveMessages(Message);
end; //TMainWnd.HandleUSRMessage

//Shows the main form after a double click on the application
procedure TMainWnd.TrayIconDblClick(Sender: TObject);
begin
    Self.Show;
end; //TMainWnd.TrayIconDblClick

//Connects the appropriate popup menu to the tray icon
procedure TMainWnd.TrayIconMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    //if menus are not supressed (form is hidden)
    if not Self.fSupressTrayMenus
    then begin
        //for left-button click
        if (Button = mbLeft)
        then begin
            //connecting disk ejection menu
            Self.TrayIcon.PopUpMenu := Self.TrayDisksPopupMenu;
            //TODO: add info to the menu and add click handlers
        end
        //for right-button click
        else if (Button = mbRight)
        then begin
            //attaching application popup menu
            Self.TrayIcon.PopUpMenu := Self.TrayAppPopupMenu;
            //TODO: add info to the menu
            //TODO: add images to image list and use them
        end;
    end;
end; //TMainWnd.TrayIconMouseDown

//Closing handling. If the option "Exit On Close" is set,
//then the aplication's window will be closed. Otherwise,
//the form will be hidden to the tray
procedure TMainWnd.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    //close app GUI
    if (ExitOnClose = true)
    then begin
        CanClose := true;
    end //then - ExitOnClose is true
    //hide to tray
    else begin
        CanClose := false;
        Self.Hide;
    end; //else - ExitOnClose is false
end; //FormCloseQuery

//Changing the selection and checking the Data pointers
procedure TMainWnd.DeviceTreeViewSelectionChanged(Sender: TObject);
begin
    //if the selection is not lost
    if Assigned(Self.DeviceTreeView.Selected)
    then begin
        //if data is assigned, it means that we are on the device node
        if Assigned(Self.DeviceTreeView.Selected.Data)
        then begin
            Self.DeviceTreeView.PopupMenu := Self.TreeViewPopupMenu;
            Self.TBEject.Enabled := true;
            Self.MICommandEject.Enabled := true;
        end
        //else it's a mount point node
        else begin
            Self.DeviceTreeView.PopupMenu := nil;
            Self.TBEject.Enabled := false;
            Self.MICommandEject.Enabled := false;
        end;
    end;
end; //TMainWnd.DeviceTreeViewSelectionChanged

//Form creation event handler. Creates the form controller
procedure TMainWnd.FormCreate(Sender: TObject);
begin
    controller := TMainFormController.Create(self);
    controller.Refresh;
    Self.fSupressTrayMenus := false;
end; //TMainWnd.FormCreate

//Form destruction event handler. Destroys the controller
procedure TMainWnd.FormDestroy(Sender: TObject);
begin
    controller.Destroy;
end; //TMainWnd.FormDestroy

//Form hiding handler. Hides the main form and shows the program
//icon in the tray area
procedure TMainWnd.FormHide(Sender: TObject);
begin
    Self.fSupressTrayMenus := false;
end; //TMainWnd.FormHide

//Form showing handler. Shows the main form and hides the program
//icon in the tray area
procedure TMainWnd.FormShow(Sender: TObject);
begin
    Self.fSupressTrayMenus := true;
end; //TMainWnd.FormShow

//Ejects the device (through controller)
procedure TMainWnd.MICommandEjectClick(Sender: TObject);
begin
    //if the selection is ready
    if Assigned(Self.DeviceTreeView.Selected)
    then begin
        //if it is a device node
        if Assigned(Self.DeviceTreeView.Selected.Data)
        then begin
            controller.RemoveDrive(Self.DeviceTreeView.Selected.Data);
        end;
    end;
end; //TMainWnd.MICommandEjectClick

//Stops the execution of application
procedure TMainWnd.MICommandExitClick(Sender: TObject);
begin
  //TODO: settings or state serialization
  ExitOnClose := true;
  self.Close;
end; //TMainWnd.MICommandExitClick

//Shows the information about the application
procedure TMainWnd.MIHelpAboutClick(Sender: TObject);
begin
    //TODO: put a box with about information
end; //TMainWnd.MIHelpAboutClick

//Shows the help
procedure TMainWnd.MIHelpIndexClick(Sender: TObject);
begin
    Application.MessageBox('Sorry, not implemented yet :(',
        'USBRemover', MB_OK or MB_ICONEXCLAMATION);
     //TODO: put some code here
end; //TMainWnd.MIHelpIndexClick

//Switches the toolbar state
procedure TMainWnd.MIViewHideToolbarClick(Sender: TObject);
begin
    controller.SwitchToolBar;
end; //TMainWnd.MIViewHideToolbarClick

//Causes a device list refresh
procedure TMainWnd.MIViewRefreshClick(Sender: TObject);
begin
    controller.Refresh;
end; //TMainWnd.MIViewRefreshClick

//Exits the application
procedure TMainWnd.TApmExitClick(Sender: TObject);
begin
    ExitOnClose := true;
    Self.Close;
end; //TMainWnd.TApmExitClick

//Show the appplcation's main form (from the tray)
procedure TMainWnd.TApmShowMainWndClick(Sender: TObject);
begin
    Self.Show;
end; //TMainWnd.TApmShowMainWndClick


initialization
  {$I MainForm.lrs}

end.

