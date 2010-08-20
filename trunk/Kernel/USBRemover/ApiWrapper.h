/*
	ApiWrapper.h
	Started: 21.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Wrapper for Windows API functions
	Declarations and exported functions
*/

#ifndef _H_APIWRAPPER
#define _H_APIWRAPPER

#ifndef __cplusplus
#error This is a C++ library, it should be compiled with VC++
#endif

#ifndef _MSC_VER
#error This module should be compiled using Visual C++
#endif

//DLL Export definition
#define DLLEXPORT _declspec(dllexport)

//Memory Leak Detection
#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <crtdbg.h>
#endif

//Windows & SDK headers
#include <windows.h>
#include <setupapi.h>
#include <cfgmgr32.h>
#include <tchar.h>
#include <initguid.h>
#include "ntdll.h"

//STL containers
#include <map>
#include <utility>
#include <vector>

//Own classes
#include "WinAPIException.h"

//Constants
const DWORD ZERO_FLAGS = (DWORD)0; //default flags
const DWORD ZERO_HANDLE = (DWORD)0; //default handle
const DWORD DEFAULT_SIZE = (DWORD)0; //default size
const DWORD ZERO_BUFFER = (DWORD)0; //zero buffer size
const DWORD WAIT_PIPE_TIMEOUT = (DWORD)20; //pipe wait timeout
const int GUID_LENGTH = (int)40; //38 - length of the GUID in 16-x base format

//Type definitions: string pair
typedef std::pair<LPTSTR, LPTSTR> StringPair;

//Type definitions: cache for device data
typedef std::map<std::pair<DWORD, DWORD>, SP_DEVINFO_DATA> DeviceDataSet;

//Type definitions: string list
typedef std::vector<LPTSTR> StringList;

//Type definitions: string pair list
typedef std::vector<StringPair> StringPairList;

//Type definitions: integer list

/* A5DCBF10-6530-11D2-901F-00C04FB951ED */
DEFINE_GUID(GUID_DEVINTERFACE_USB_DEVICE, 0xA5DCBF10L, 0x6530, 0x11D2, 0x90, 0x1F, 0x00, \
             0xC0, 0x4F, 0xB9, 0x51, 0xED);

/*
	Added 11.08.2010 by Asha'man
*/

// For array clearing
#define DELARRAY(x) if (x != NULL) { delete [] x; x = NULL; }
// For object clearing
#define DELOBJ(x) if (x != NULL) { delete x; x = NULL; }
// For string list clearing
#define CLRSTRLIST(x) for (size_t index = 0; index<(x).size(); ++index) {DELARRAY((x)[index]);}
// For string pair list clearing
#define CLRSTRPLIST(x) for (size_t index = 0; index<(x).size(); ++index) \
	{DELARRAY((x)[index].first); DELARRAY((x)[index].second);}

//Function prototypes

HANDLE WINAPI GetDeviceInformationSet(GUID classGUID, DWORD flags);

SP_DEVINFO_DATA WINAPI GetDeviceInformation(LPTSTR idString, HANDLE setHandle);

DeviceDataSet WINAPI GetDeviceInformationCache(GUID classGUID, HANDLE setHandle);

STORAGE_DEVICE_NUMBER WINAPI GetDeviceNumber(HANDLE fileHandle);

LPTSTR WINAPI GetDeviceId(HANDLE instanceHandle);

STORAGE_BUS_TYPE WINAPI GetBusType(LPTSTR name);

bool WINAPI EjectDevice(DEVINST devInst);

LPTSTR FormatDeviceName(LPTSTR devName);

LPTSTR FormatDOSVolumeName(LPTSTR volName);

bool FilterRemovableVolume(LPTSTR path);

LARGE_INTEGER WINAPI GetVolumeSize(LPTSTR path);

DWORD WINAPI GetDiskNumber(LPTSTR path);

StringList* GetVolumeMountPoints(LPTSTR path);

PVOID GetSystemInformationTable(DWORD tableType);

DWORD GetFileHandleType();

LPTSTR GetFileName(HANDLE hFile);

LPTSTR GetProcessName (PSYSTEM_PROCESS_INFORMATION processInfo, DWORD processId);

#endif