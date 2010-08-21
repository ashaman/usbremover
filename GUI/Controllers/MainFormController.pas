unit MainFormController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Connector, Menus, ExtCtrls, MainForm;

const
    { ToolBar state labels }
    MenuToolBarLabels: array [0..1] of string =
    (
        'Show toolbar',
        'Hide toolbar'
    );
    ApplicationState: array [0..1] of string =
    (
        'Show main window',
        'Hide main window'
    );
    BalloonTitle: array [0..1] of string =
    (
        'Device ejected!',
        'Device removal failed!'
    );


    BalloonTimeout = 5;

type
    {
        Description:
            Main form controller for complex operations
    }
    TMainFormController = class(TObject)
    private
        fMainWnd: TMainWnd; //view
        fPipeConnector: TPipeConnector; //model
        fApplicationState: byte; //application state
        fToolBarState: byte; //toolbar state index

        //event handlers
        procedure OnRemovalFailed(Sender: TObject); //removal failed handler
        procedure OnRemovalSucceeded(Sender: TObject); //removal succeded handler
        //procedure OnSearchProgress(Sender: TObject); //handle search progress

    public
        constructor Create(mainWnd: TMainWnd); //constructor
        destructor Destroy; override; //destructor

        procedure Refresh; //refreshes view state
        procedure SwitchToolBar; //switches the toolbar
    end;

implementation

{EVENT HANDLERS}

//handles the device removal success and shows the info in the tray
procedure TMainFormController.OnRemovalSucceeded(Sender: TObject);
begin
    //removal succeeded - we set all the tray icon parameters
    //TODO: do it better
    Self.fMainWnd.TrayIcon.BalloonTitle := BalloonTitle[0];
    Self.fMainWnd.TrayIcon.BalloonHint := 'Device removal succeeded!';
    Self.fMainWnd.TrayIcon.BalloonTimeout := BalloonTimeout;
    Self.fMainWnd.TrayIcon.BalloonFlags := bfInfo;
    Self.fMainWnd.TrayIcon.ShowBalloonHint;
    //TODO: add what device was ejected (how? - save state in the controller?)
end; //TMainFormController.OnRemovalSucceeded

//handles the device removal failure and shows the handle search form
procedure TMainFormController.OnRemovalFailed(Sender: TObject);
begin
    //removal failed - we set all the tray icon parameters
    //TODO: do it better
    Self.fMainWnd.TrayIcon.BalloonTitle := BalloonTitle[1];
    Self.fMainWnd.TrayIcon.BalloonHint := 'Device removal failed!';
    Self.fMainWnd.TrayIcon.BalloonTimeout := BalloonTimeout;
    Self.fMainWnd.TrayIcon.BalloonFlags := bfError;
    Self.fMainWnd.TrayIcon.ShowBalloonHint;
    //TODO: add progress form!!!
    //TODO: add what device was ejected (how? - save state in the controller?)
end; //TMainFormController.OnRemovalFailed

{END EVENT HANDLERS}

{
    Purpose:
        Switches the toolbar's state between "Shown" and "Hidden"
    Parameters:
        None
    Return value:
        None
}
procedure TMainFormController.SwitchToolBar;
begin
    //toolbar is shown; index is greater than zero
    if (Self.fToolBarState > 0)
    then begin
        //hiding the toolbar
        Self.fMainWnd.ToolBar.Hide;
        //decreasing the index
        Dec(Self.fToolBarState);
    end
    else begin
        //showing the toolbar
        Self.fMainWnd.ToolBar.Show;
        //increasing the index
        Inc(Self.fToolBarState);
    end;
    //setting the label in the menu
    Self.fMainWnd.MIViewHideToolbar.Caption :=
        MenuToolBarLabels[Self.fToolBarState];
end; //TMainFormController.SwitchToolBar

{
    Purpose:
        Refreshes the view by getting the device information
        from the service (throug the TPipeConenctor class)
    Parameters:
        None
    Return value:
        None
}
procedure TMainFormController.Refresh;
begin
    //refreshing the device list
    fPipeConnector.Refresh;
end; //TMainFormController.Refresh

{
    Purpose:
        Initializes a new instance of controller and passes
        the main form as an argument
    Parameters:
        mainWnd - main window pointer
}
constructor TMainFormController.Create(mainWnd: TMainWnd);
begin
    inherited Create;
    Self.fMainWnd := mainWnd;
    //creating the connector
    Self.fPipeConnector := TPipeConnector.Create;
    //toolbar is shown
    Self.fToolBarState := 1;
    Self.fPipeConnector.OnRemomalFailed := @Self.OnRemovalFailed;
end; //constructor

{
    Purpose:
        Destructor
}
destructor TMainFormController.Destroy;
begin
    //destroying the connector object
    Self.fPipeConnector.Destroy;
    inherited Destroy;
end; //destructor

end.

