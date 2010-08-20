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
  Menus, ExtCtrls, ComCtrls, PopupNotifier, LCLType,
  Settings, Connector ; //application settings

type


    //TODO: implement form controller

  { TMainWnd }

  TMainWnd = class(TForm)
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
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
    TrayAppPopupMenu: TPopupMenu;
    TrayDisksPopupMenu: TPopupMenu;
    ToolBar: TToolBar;
    TrayIcon: TTrayIcon;
    DeviceTreeView: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainWnd: TMainWnd;

implementation

var
   pconnector: TPipeConnector;

{ TMainWnd }

//Shows the main form after a double click on the application
procedure TMainWnd.TrayIconDblClick(Sender: TObject);
begin
    Self.Show;
end; //TMainWnd.TrayIconDblClick

//Connects the appropriate popup menu to the tray icon
procedure TMainWnd.TrayIconMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
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

//Form creation event handler
procedure TMainWnd.FormCreate(Sender: TObject);
begin
    //creating the connector
    pconnector := TPipeConnector.Create;
    //refreshing the device list
    pconnector.Refresh;
end; //TMainWnd.FormCreate

//Form destruction event handler
procedure TMainWnd.FormDestroy(Sender: TObject);
begin
    //calling the connector's destructor
    pconnector.Destroy;
end; //TMainWnd.FormDestroy

//Form hiding handler. Hides the main form and shows the program
//icon in the tray area
procedure TMainWnd.FormHide(Sender: TObject);
begin
  TrayIcon.Show;
end; //TMainWnd.FormHide

//Form showing handler. Shows the main form and hides the program
//icon in the tray area
procedure TMainWnd.FormShow(Sender: TObject);
begin
  TrayIcon.Hide;
end; //TMainWnd.FormShow

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

procedure TMainWnd.MIViewHideToolbarClick(Sender: TObject);
begin
    //TODO: do it better - with string constants (lists, etc.)
     if (self.ToolBar.Visible)
     then begin
          self.ToolBar.Hide;
          self.MIViewHideToolbar.Caption := 'Show toolbar';
     end
     else begin
          self.ToolBar.Show;
          self.MIViewHideToolbar.Caption := 'Hide toolbar';
     end;
end;

//Causes a device list refresh
procedure TMainWnd.MIViewRefreshClick(Sender: TObject);
begin
    //refreshing the device list
    pconnector.Refresh;
end; //TMainWnd.MIViewRefreshClick

//Exits the application
procedure TMainWnd.TApmExitClick(Sender: TObject);
begin
    ExitOnClose := true;
    self.Close;
end; //TMainWnd.TApmExitClick

procedure TMainWnd.TApmShowMainWndClick(Sender: TObject);
begin

end;

initialization
  {$I MainForm.lrs}

end.

