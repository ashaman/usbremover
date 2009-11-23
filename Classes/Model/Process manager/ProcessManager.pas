unit ProcessManager;

interface

uses
Windows, Classes, Messages, WbemScripting_TLB, OleServer, ActiveX;

type
TProccesManager = class(TObject);
SWbemLocator1: TSWbemLocator;
  private
    fEventHandlers: TList;
  protected
    fProcesses: TList;
    constructor Create;
    procedure NotifyAll;

  public
    destructor Destroy; override;
    function GetProcesses: TProcess;
    procedure StopProcess(ProcID: HWND);
    function GetBlockerID: HWND;

    //These functions add event handlers from listeners
    procedure AddHandler(Handler: TNotifyEvent);
    procedure RemoveHandler(Handler: TNotifyEvent);
  end;

{==============================================================================}
implementation

uses
  SysUtils, WMI, ShellObjExtended;


implementation

end.
 