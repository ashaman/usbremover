{
    Communication.pas
    Started: 16.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Defines basic structures for IPC.
    Contains common procedures
}
unit Communication;

{$mode objfpc}{$H+}
interface

uses
    SysUtils, Windows;

const
     //communication pipes names
     //all is vice versa (compare to server side)
     PIPE_OUTGOING_NAME: WideString = '\\.\pipe\usbremover_inc';
     PIPE_INCOMING_NAME: WideString = '\\.\pipe\usbremover_out';

     //mount points separator
     MTPNT_SEPARATOR: WideChar = '?';
     MAXSTRINGLEN: DWORD = MAX_PATH+1;

type

{ Unicode char buffer}
TWideCharArray = array[0..MAX_PATH] of WideChar;

  { Message structure }
  OPINFO = record
      //operation code
      dwOpCode: DWORD;
      //operation status
      dwOpStatus: DWORD;
  end;
  POPINFO = ^OPINFO;

  { Device index information }
  DEVINDEX = record
    	//device level: 1-2-3
      dwDeviceLevel: DWORD;
     	//device number by order
      dwDeviceNumber: SIZE_T;
     	//top device index
      dwTopIndex: SIZE_T;
  end;
  PDEVINDEX = ^DEVINDEX;

  { Device information }
  DEVINFO = record
      //indexes information
      devIndex: DEVINDEX;
      //parent number
      dwParent: SIZE_T;
      //device description
      description: TWideCharArray;
      //name
      name: TWideCharArray;
      //actual mount points count
      MountPtsCount: SIZE_T;
  end;
  PDEVINFO = ^DEVINFO;


  { Process-locker info }
  PROC_INFO = record
      //process id
      dwId: DWORD;
      //process name
      name: TWideCharArray;
      //file count
      LockedFilesCount: SIZE_T;
  end;
  PPROC_INFO = ^PROC_INFO;

{ Operation codes }
const
    OPERATION_START = 1;
    OPERATION_PROGRESS = 2;
    OPERATION_FINISH = 3;

//Device codes
    DEVICE_REFRESH_REQUEST = 1; //O
    DEVICE_REFRESH_ANSWER = 2; //I

//Ejection codes
    DEVICE_EJECT_REQUEST = 10; //O
    DEVICE_EJECT_ACCEPT = 11; //I
    DEVICE_EJECT_REJECT = 12; //I
    DEVICE_EJECT_FORCED = 13; //O

//Process control codes
    PROCESS_SEARCH_HANDLES = 20; //I
    PROCESS_KILL_PROCESS = 21; //O
    PROCESS_KILL_ACCEPT = 22; //I
    PROCESS_KILL_REJECT = 23; //I
    PROCESS_LOCK_INFO = 24; //I

// Windows messages
    WM_USBREMOVER = WM_USER + 1;
    //indicates that the window should be refreshed
    WM_REFRESH_ANSWER = WM_USER + DEVICE_REFRESH_ANSWER;
    //tells about the device's successful ejection
    WM_EJECT_ACCEPT = WM_USER + DEVICE_EJECT_ACCEPT;
    //tells about the device's ejection failure
    WM_EJECT_REJECT = WM_USER + DEVICE_EJECT_REJECT;
    //indicates that the locker information is ready
    WM_LOCK_INFO = WM_USER + PROCESS_LOCK_INFO;
    //percentage of execution
    WM_PERCENTAGE = WM_LOCK_INFO + 1;
    //show lock form
    WM_SHOW_LOCK_FORM = WM_PERCENTAGE + 1;

function WideToUTF8String(Text: WideString): String; inline;

implementation

{
    Purpose:
        Converts WideString to 1-byte UTF-8 string
    Parameters:
        text - WideString (2-byte UTF-16)
    Return value:
        1-byte UTF-8 string
}
function WideToUTF8String(Text: WideString): String; inline;
var
    stringLength: integer; //string length
begin
    //preparing string buffer
    stringLength := Length(Text);
    SetLength(Result, MAX_PATH + 1);
    ZeroMemory(PChar(@Result[1]), sizeof(char)*(MAX_PATH + 1));
    //converting chars (to UTF8 charset)
    WideCharToMultiByte(CP_UTF8, 0, PWideChar(@Text[1]),
        stringLength, PChar(@Result[1]), MAX_PATH*sizeof(char), nil, nil);
    //trimming unnecessary symbols
    Result := TrimRight(Result);
end; //WideToUTF8String



end.

