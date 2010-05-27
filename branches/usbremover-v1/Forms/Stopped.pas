{
  UI PART.
  DO NOT MODIFY.
  WRITTEN BY J.L.Blackrow
}
unit Stopped;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type
  TStoppedForm = class(TForm)
    LabelPanel: TPanel;
    TreeViewProcesses: TTreeView;
    ButtonPanel: TPanel;
    ButtonRetry: TButton;
    ButtonForced: TButton;
    ButtonStopProcess: TButton;
    LabelInformation: TLabel;
    ButtonCancel: TButton;
    ProgressBar: TProgressBar;
    LabelProgress: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeViewProcessesClick(Sender: TObject);
    procedure ButtonRetryClick(Sender: TObject);
    procedure ButtonForcedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StoppedForm: TStoppedForm;

implementation

{$R *.dfm}

uses
  StoppedFormController;

var
  controller: TStoppedFormController;

//Closes the form
procedure TStoppedForm.ButtonCancelClick(Sender: TObject);
begin
  Close;
end; //ButtonCancelClick

//Attaches the controller
procedure TStoppedForm.FormCreate(Sender: TObject);
begin
  controller := TStoppedFormController.Create(self);
end; //FormCreate

//Executes the operations when form is closed:
//shows the previous form
procedure TStoppedForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  controller.Close;
end; //FormClose

//Handles clicks on the TreeView
procedure TStoppedForm.TreeViewProcessesClick(Sender: TObject);
begin
  //checking if there are any available elements
  if Assigned(TreeViewProcesses.Selected)
  then begin
    //checking if it is a root element
    if not Assigned(TreeViewProcesses.Selected.Parent)
    then begin
      ButtonStopProcess.Enabled := true;
      Exit;
    end; //then - root element chosen
  end;  //then - some elements present
  ButtonStopProcess.Enabled := false;
end; //TreeViewProcessesClick

//Tries to remove the device again
procedure TStoppedForm.ButtonRetryClick(Sender: TObject);
begin
  controller.TryEject;
end; //ButtonRetryClick

//Makes a forced removal of the device
procedure TStoppedForm.ButtonForcedClick(Sender: TObject);
begin
  controller.ForcedRemoval;
end; //ButtonForcedClick

end.
