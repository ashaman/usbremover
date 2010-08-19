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
  Menus, ExtCtrls, ComCtrls, PopupNotifier,
  Settings, Connector ; //application settings

type

  { TMainWnd }

  TMainWnd = class(TForm)
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MIViewHideToolbar: TMenuItem;
    MIViewRefresh: TMenuItem;
    MIHelpAbout: TMenuItem;
    MIHelpIndex: TMenuItem;
    MICommand: TMenuItem;
    MIView: TMenuItem;
    MIHelp: TMenuItem;
    MICommandExit: TMenuItem;
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
    procedure TrayIconDblClick(Sender: TObject);
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
  Show;
end; //TMainWnd.TrayIconDblClick

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

procedure TMainWnd.FormCreate(Sender: TObject);
begin
    pconnector := TPipeConnector.Create;
end;

procedure TMainWnd.FormDestroy(Sender: TObject);
begin
    pconnector.Destroy;
end;

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

procedure TMainWnd.MIHelpAboutClick(Sender: TObject);
begin
     //TODO: put some code here
end;

procedure TMainWnd.MIHelpIndexClick(Sender: TObject);
begin
      pconnector.EjectDevice(pconnector.Devices[0].Index);
end;

procedure TMainWnd.MIViewHideToolbarClick(Sender: TObject);
begin
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

procedure TMainWnd.MIViewRefreshClick(Sender: TObject);
begin
    pconnector.Refresh;
end;

initialization
  {$I MainForm.lrs}

end.

