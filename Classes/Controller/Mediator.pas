{
  THIS UNIT IS A PART OF CONTROLLER.
  DO NOT MODIFY.
}
unit Mediator;

interface

uses
  Device, MainFormController, StoppedFormController;

type
  TMediator = class(TObject)
  private
    fMainFormController: TMainFormController;
    fStoppedFormController: TStoppedFormController;
    constructor Create;
  public
    class function GetInstance: TMediator;
    procedure AttachController(Controller: TObject);
    procedure CloseStoppedForm(Device: TDevice);
    procedure CloseStoppedFormExplicitly;
    procedure ShowMainForm;
    procedure ShowStoppedForm(Device: TDevice);
  end;

implementation

uses
  Windows, Stopped, Main, Forms, USBManager;

var
  Instance: TMediator;

procedure TMediator.CloseStoppedFormExplicitly;
begin
  fStoppedFormController.ForcedClose;
end; //CloseStoppedFormExplicitly

//Shows main form
procedure TMediator.ShowMainForm;
begin
  fMainFormController.Show;
end; //ShowMainForm

//Attaches controllers to the mediator
procedure TMediator.AttachController(Controller: TObject);
begin
  if Controller is TMainFormController
  then begin
    fMainFormController := Controller as TMainFormController;
  end
  else if Controller is TStoppedFormController
  then begin
    fStoppedFormController := Controller as TStoppedFormController;
  end
end; //AttachController

//This function shows the "StoppedForm"
procedure TMediator.ShowStoppedForm(Device: TDevice);
begin
  fMainFormController.Hide;
  fStoppedFormController.Device := Device;
  fStoppedFormController.ShowForm;
end; //ShowStoppedForm

//This function closes the "StoppedForm"
procedure TMediator.CloseStoppedForm;
begin
  fStoppedFormController.Close;
end; //CloseStoppedForm

//Singleton method
class function TMediator.GetInstance: TMediator;
begin
  if not Assigned(Instance)
  then begin
    Instance := TMediator.Create;
  end;
  Result := Instance;
end; //GetInstance

{CONSTRUCTOR}
constructor TMediator.Create;
begin
  inherited Create;
end; //Create

end.
