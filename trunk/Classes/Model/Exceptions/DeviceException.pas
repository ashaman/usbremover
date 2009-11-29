{
  Device class exception
}

unit DeviceException;

interface
uses
  SysUtils;
type
  EDeviceException = class(Exception)
  private
    fInnerException: Exception;
    function GetInnerException: Exception;
  public
    constructor Create(msg: string); overload;
    constructor Create(innerException: Exception; info: string); overload;
    property InnerException: Exception read GetInnerException;
  end;

implementation

function EDeviceException.GetInnerException: Exception;
begin
  Result := fInnerException;
end;

constructor EDeviceException.Create(innerException: Exception; info: string);
begin
  inherited Create(info);
  fInnerException := innerException;
end;

constructor EDeviceException.Create(msg: string);
begin
  inherited Create(msg);
end;

end.


