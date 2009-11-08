unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, USBManager;

type
  TMainFrm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

procedure TMainFrm.FormCreate(Sender: TObject);
var
  rm: TUSBManager;
begin
  rm := TUSBManager.GetManager;
  rm.RemoveDrive(0);
end;

end.
