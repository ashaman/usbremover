{
    Mediator.pas
    Started: 27.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Blocked files form controller declaration and implementation
}
unit Mediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MainFormController, MainForm, //main form
  BlockedFilesFormController, BlockedFilesForm, //blocked form
  Connector; //pipe connector

type
    TMediator = class(TObject)
    private
        fMainFormController: TMainFormController; //main form controller
        fBlockedFormController: TBlockedFilesFormController; //blocked form ctrl
        fPipeConnector: TPipeConnector; //pipe connector
    public

        constructor Create;
        destructor Destroy; override;
    end;

implementation

{
    Purpose:
        Constructor. Creates all controllers and connectors
    Parameters:
        None
}
constructor TMediator.Create;
begin
    inherited Create;
    //creating the connector
    Self.fPipeConnector := TPipeConnector.Create;
    //TODO: create controllers and attach views
end; //constructor

{
    Purpose:
        Releases all relevant resources
}
destructor TMediator.Destroy;
begin
    Self.fPipeConnector.Destroy;
    inherited Destroy;
end; //destructor

end.

