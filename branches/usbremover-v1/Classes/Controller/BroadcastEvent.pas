{
  TBroadcastEvent and TBroadcastNotifyEvent
  Uploader: Jerome Tremblay
  Updated by: J.L. Blackrow
  Company: Popup! Solutions
}
unit BroadcastEvent;

interface

uses
  Classes, SysUtils;

type
  TBroadcastEvent = class
  private
    FObservers: TList;
  protected
    function FindObserver(Observer: TMethod): integer;
    function GetObserver(Index: integer): TMethod;
    procedure SignalObserver(Observer: TMethod); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Attach(Observer: TMethod);
    procedure Detach(Observer: TMethod);

    procedure Signal;
  end;

  TBroadcastNotifyEvent = class(TBroadcastEvent)
  private
    FSender: TObject;
  protected
    procedure SignalObserver(Observer: TMethod); override;
  public
    procedure Attach(Observer: TNotifyEvent);
    procedure Detach(Observer: TNotifyEvent);

    procedure Signal(Sender: TObject);
  end;

implementation

{ TEvent }

procedure TBroadcastEvent.Attach(Observer: TMethod);
var
  Index: integer;
begin
  Index := FindObserver(Observer);

  { This assertion is facultative, we could just ignore observers  }
  { already attached, but it's a good way to detect problems early }
  { and avoid unnecessary processing.                              }

  Assert(Index < 0, 'This observer was already attached to this event');

  { A method contains two pointers:                              }
  { - The code pointer, that's where the procedure is in memory  }
  { - The data pointer, this tells Delphi what instance of the   }
  {   object calls the procedure                                 }
  { We must store both pointers in order to use that callback.   }

  if Index < 0 then
    begin
    FObservers.Add(Observer.Code);
    FObservers.Add(Observer.Data);
    end;
end;

constructor TBroadcastEvent.Create;
begin
  inherited;
  FObservers := TList.Create;
end;

destructor TBroadcastEvent.Destroy;
begin
  { This assertion is facultative, but I prefer when all my objects }
  { are "clean" when they are destroyed.                            }
  Assert(FObservers.Count = 0, 'Not all observers were detached');
  FreeAndNil(FObservers);
  inherited;
end;

procedure TBroadcastEvent.Detach(Observer: TMethod);
var
  Index: integer;
begin
  Index := FindObserver(Observer) * 2;

  { Again, the assertion is facultative, nothing would be broken }
  { if we just ignored it.                                       }
  Assert(Index >= 0, 'The observer was not attached to this event');

  if Index >= 0 then
    begin
    FObservers.Delete(Index); // Delete code pointer
    FObservers.Delete(Index); // Delete data pointer
    end;
end;

function TBroadcastEvent.FindObserver(Observer: TMethod): integer;
var
  i: integer;
begin
  { Search fails by default, if there is a match, result will be updated. }
  Result := -1;
  if FObservers.Count = 0
  then begin
    Exit;
  end; //fixed by J.L.Blackrow
  for i := (FObservers.Count div 2)-1 downto 0 do
    begin
    { We have a match only if both the Code and Data pointers are the same. }
    if (Observer.Code = FObservers[i * 2 ]) and (Observer.Data = FObservers[i * 2 + 1]) then
      begin
      Result := i;
      break;
      end;
    end;
end;

function TBroadcastEvent.GetObserver(Index: integer): TMethod;
begin
  { Fill the TMethod record with the code and data pointers. }
  Result.Code := FObservers[Index * 2];
  Result.Data := FObservers[Index * 2 + 1];
end;

procedure TBroadcastEvent.SignalObserver(Observer: TMethod);
begin
  { Descendants must take care to notify the Observer by themselves }
  { because we cannot know the parameters required by the event.    }

  Assert(Assigned(@Observer));
  { We could make this method Abstract and force descendants, but   }
  { I prefer to do a run-time check to validate the passe methods   }
end;


procedure TBroadcastEvent.Signal;
var
  i: integer;
begin
  { Call SignalObserver for each stored observers in reverse order. }

  { SignalObserver (which is declared in sub-classes) will typecast }
  { the TMethod record into whatever procedure type it handles.     }
  { See the TBroadcastNotifyEvent below for an example.                 }

  for i := (FObservers.Count div 2)-1 downto 0 do
    begin
    SignalObserver(GetObserver(i));
    end;
end;

{ TBroadcastNotifyEvent }

procedure TBroadcastNotifyEvent.Attach(Observer: TNotifyEvent);
begin
  inherited Attach(TMethod(Observer));
  //Observer(nil); //fixed by J.L. Blackrow
end;

procedure TBroadcastNotifyEvent.Detach(Observer: TNotifyEvent);
begin
  inherited Detach(TMethod(Observer));
end;

procedure TBroadcastNotifyEvent.Signal(Sender: TObject);
begin
  FSender := Sender;
  inherited Signal;
end;

procedure TBroadcastNotifyEvent.SignalObserver(Observer: TMethod);
begin
  inherited;
  TNotifyEvent(Observer)(FSender);
end;

end.
