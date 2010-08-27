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

    end;

implementation

end.

