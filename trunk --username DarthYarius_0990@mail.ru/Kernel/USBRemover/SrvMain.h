/*
	SrvMain.h
	Started: 20.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Service declarations for USBRemover
*/

#ifndef _H_SRVMAIN
#define _H_SRVMAIN

/*
	The following headers are needed to support
	the Windows Service Application
*/
#include <windows.h>
#include <tchar.h>

#include "ServiceDispatcher.h"

/*
	The following constants and variables are needed for
	the correct work of the Windows Service Application
*/

/*
	Global variables. Defined in SrvMain.cpp
*/
extern DWORD dwErrorCode; //error code
extern SERVICE_STATUS serviceStatus; //status of service work
extern SERVICE_STATUS_HANDLE serviceStatusHandle; //handle to the status of service

/*
	Constants
*/
const LPTSTR serviceName = _T("USBRemover"); //service application name
const LPTSTR displayName = _T("USBRemover kernel service"); //name displayed in the Services console
const DWORD SERVICE_RETURN_CODE = (DWORD)0; //service status exit code
const DWORD WAIT_INITIAL_TIME = (DWORD)3000;
const DWORD DEFAULT_CHECKPOINT = (DWORD)0;
const DWORD DEFAULT_WAIT_HINT = (DWORD)0;

/*
	The following section consists of the function
	prototypes needed for the service to run
*/
void WINAPI ServiceMain(DWORD argc, LPTSTR* argv); //main service routine

#endif
