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

{$ALIGN 8}

uses
    SysUtils, Windows;

const
     //communication pipe name
     PIPE_NAME: WideString = '\\.\pipe\usbremover';
     //mount points separator
     MTPNT_SEPARATOR: WideChar = '?';
     MAXSTRINGLEN: DWORD = MAX_PATH+1;

type

{ Unicode char buffer}
LPWSTR = array[0..MAX_PATH] of WideChar;

{ Message structure }
OPINFO = packed record
       //operation code
       dwOpCode: DWORD;
       //operation status
       dwOpStatus: DWORD;
end;
POPINFO = ^OPINFO;

{ Device index information }
DEVINDEX = packed record
	//device level: 1-2-3
    dwDeviceLevel: DWORD;
	//device number by order
    dwDeviceNumber: DWORD;
	//top device index
    dwTopIndex: DWORD;
end;
PDEVINDEX = ^DEVINDEX;

{ Device information }
DEVINFO = packed record
	//indexes information
	devIndex: DEVINDEX;
	//parent number
	dwParent: DWORD;
	//device description
    description: LPWSTR;
    //name
    name: LPWSTR;
	//actual mount points count
    dwMountPtsCount: DWORD;
end;
PDEVINFO = ^DEVINFO;

{ Process-locker info }
PROC_INFO = packed record
	//process id
    dwId: DWORD;
	//process name
    name: LPWSTR;
	//file count
	dwLockedFilesCount: DWORD;
end;
PPROC_INFO = ^PROC_INFO;

{ Operation codes }
const
    OPERATION_START = 1;
    OPERATION_PROGRESS = 2;
    OPERATION_FINISH = 3;

//Device codes
    DEVICE_REFRESH_REQUEST = 1; //I
    DEVICE_REFRESH_ANSWER = 2; //O

//Ejection codes
    DEVICE_EJECT_REQUEST = 10; //I
    DEVICE_EJECT_ACCEPT = 11; //O
    DEVICE_EJECT_REJECT = 12; //O
    DEVICE_EJECT_FORCED = 13; //I

//Process control codes
    PROCESS_SEARCH_HANDLES = 20; //O
    PROCESS_KILL_PROCESS = 21; //I
    PROCESS_KILL_ACCEPT = 22; //O
    PROCESS_KILL_REJECT = 23; //O
    PROCESS_LOCK_INFO = 24; //O


implementation

end.

