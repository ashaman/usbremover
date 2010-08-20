/*
	Communication.h
	Started: 13.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Communication code definitions
*/

//TODO: change the DWORD sizes to size_t

#ifndef _H_COMMUNICATION
#define _H_COMMUNICATION

#include <windows.h>

//communication pipes names
#define PIPE_INCOMING_NAME _T("\\\\.\\pipe\\usbremover_inc")
#define PIPE_OUTGOING_NAME _T("\\\\.\\pipe\\usbremover_out")

//mount points separator
#define MTPNT_SEPARATOR _T('?');
//maximal string length
#define MAXSTRINGLEN MAX_PATH+1

/*
	Message structure
*/
typedef struct _OPINFO
{
	//operation code
	DWORD dwOpCode;
	//operation status
	DWORD dwOpStatus;
} OPINFO, *POPINFO;

/*
	Device index information
*/
typedef struct _DEVINDEX
{
	//device level: 1-2-3
	DWORD dwDeviceLevel;
	//device number by order
	DWORD dwDeviceNumber;
	//top device index
	DWORD dwTopIndex;
} DEVINDEX, *PDEVINDEX;

/*
	Device information
*/
typedef struct _DEVINFO
{
	//indexes information
	DEVINDEX devIndex;
	//parent number
	DWORD dwParent;
	//device description
	TCHAR description[MAXSTRINGLEN];
	//name
	TCHAR name[MAXSTRINGLEN];
	//actual mount points count
	DWORD dwMountPtsCount;
} DEVINFO, *PDEVINFO;

/*
	Process-locker info
*/
typedef struct _PROC_INFO
{
	//process id
	DWORD dwId;
	//process name
	TCHAR name[MAXSTRINGLEN];
	//file count
	DWORD dwLockedFilesCount;
} PROC_INFO, *PPROC_INFO;

/*
	Operation codes
*/

//Operation status codes
#define OPERATION_START 1
#define OPERATION_PROGRESS 2
#define OPERATION_FINISH 3

//Device codes
#define DEVICE_REFRESH_REQUEST 1 //I
#define DEVICE_REFRESH_ANSWER 2 //O

//Ejection codes
#define DEVICE_EJECT_REQUEST 10 //I
#define DEVICE_EJECT_ACCEPT 11 //O
#define DEVICE_EJECT_REJECT 12 //O
#define DEVICE_EJECT_FORCED 13 //I

//Process control codes
#define PROCESS_SEARCH_HANDLES 20 //O
#define PROCESS_KILL_PROCESS 21 //I
#define PROCESS_KILL_ACCEPT 22 //O
#define PROCESS_KILL_REJECT 23 //O
#define PROCESS_LOCK_INFO 24 //O


#endif