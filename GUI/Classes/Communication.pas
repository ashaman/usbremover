{
    Communication.pas
    Started: 16.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Defines basic structures for IPC
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
      dwDeviceNumber: DWORD;
     	//top device index
      dwTopIndex: DWORD;
  end;
  PDEVINDEX = ^DEVINDEX;

  { Device information }
  DEVINFO = record
      //indexes information
      devIndex: DEVINDEX;
      //parent number
      dwParent: DWORD;
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


implementation

end.

