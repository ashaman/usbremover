/*
	SrvMain.cpp
	Started: 20.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Service implementation for USBRemover
*/

/*
	IMPORTANT NOTE!
		If anything else is needed for this service,
		look through the RSDN article "Programming Services"
*/

#include <stdio.h>
#include "SrvMain.h"

#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
_CrtMemState s1, s2, s3;
#endif

/*
	Forward declaration of functions
*/
DWORD WINAPI ServiceControl(
	DWORD dwControlCode,
	DWORD dwEventType,
	LPVOID lpEventData,
	LPVOID lpContext);
VOID WINAPI ReportServiceStatus(
	DWORD dwCurrentState,
	DWORD dwWin32ExitCode,
	DWORD dwWaitHint = DEFAULT_WAIT_HINT,
	DWORD dwControlsAccepted = SERVICE_ACCEPT_STOP);

/*
	Explicit variable creation
*/
SERVICE_STATUS_HANDLE serviceStatusHandle; //handle to service status
SERVICE_STATUS serviceStatus; //service status
ServiceDispatcher *dispatcher; //service dispatcher

/*
	Purpose: 
		Entry point for the service
	Parameters:
		argc - Number of arguments in the lpszArgv array
		argv - Array of strings. The first string is the name of
		the service and subsequent strings are passed by the process
		that called the StartService function to start the service.
	Return value:
		None
*/

VOID WINAPI ServiceMain(DWORD argc, LPTSTR* argv)
{
	//Setting the flags and fields

	//Registering service and SCM requests handler
	serviceStatusHandle = RegisterServiceCtrlHandlerEx(serviceName, ServiceControl, NULL);
	if (!serviceStatusHandle)
	{
		throw WinAPIException(GetLastError());
		return;
	}

	//Set the initial status and send it to the SCM
	serviceStatus.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
	ReportServiceStatus(SERVICE_START_PENDING, NO_ERROR,
		WAIT_INITIAL_TIME, SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN | SERVICE_ACCEPT_PAUSE_CONTINUE);

	/* 
		Initializing core of the service
	*/
#ifdef DEBUG
	_CrtMemCheckpoint(&s1);
#endif
	dispatcher = new ServiceDispatcher(serviceStatusHandle,
		DEVICE_NOTIFY_SERVICE_HANDLE);

	//Set the status to Running
	ReportServiceStatus(SERVICE_RUNNING, NO_ERROR);

	//Here - the work of service! It works until the "STOP" command is received
	dispatcher->HandleRequests(serviceStatus);

}


/*
	Purpose: 
		Called by SCM whenever a control code is sent to the service
		using the ControlService function.
	Parameters:
		dwControlCode - control code received from the SCM
		dwEventType - The type of event that has occurred. This parameter 
		is used if dwControl is SERVICE_CONTROL_DEVICEEVENT, 
		SERVICE_CONTROL_HARDWAREPROFILECHANGE, SERVICE_CONTROL_POWEREVENT, 
		or SERVICE_CONTROL_SESSIONCHANGE. Otherwise, it is zero.
		lpEventData - Additional device information, if required
		lpContext - User-defined data passed from RegisterServiceCtrlHandlerEx
	Return value:
		Service error state
*/
DWORD WINAPI ServiceControl(DWORD dwControlCode, DWORD dwEventType,
						   LPVOID lpEventData, LPVOID lpContext)
{
	//Setting the initial value of status
	DWORD dwSvcStatus = serviceStatus.dwCurrentState;
	
	switch (dwControlCode)
	{
	case SERVICE_CONTROL_PAUSE:
		{
			//Pausing service
			ReportServiceStatus(SERVICE_PAUSE_PENDING, NO_ERROR);
			dispatcher->Pause();
			dwSvcStatus = SERVICE_PAUSED;
			break;
		}
	case SERVICE_CONTROL_CONTINUE:
		{
			//Resuming service
			ReportServiceStatus(SERVICE_CONTINUE_PENDING, NO_ERROR);
			dispatcher->Resume();
			dwSvcStatus = SERVICE_RUNNING;
			break;
		}
	case SERVICE_CONTROL_STOP:
	case SERVICE_CONTROL_SHUTDOWN:
		{
			//Stopping service (manually or on shutdown)
			ReportServiceStatus(SERVICE_STOP_PENDING, NO_ERROR);
			//setting new status
			dwSvcStatus = SERVICE_STOPPED;
			//deleting dispatcher
			ServiceDispatcher::TerminateDispatcherThread(dispatcher);
#ifdef DEBUG
			_CrtMemCheckpoint(&s2);
			if (_CrtMemDifference(&s3, &s1, &s2))
				_CrtMemDumpStatistics(&s3);
			_CrtDumpMemoryLeaks();
#endif

			break;
		}
	case SERVICE_CONTROL_DEVICEEVENT:
		{
			//Handling device events
			dispatcher->DispatchEvent(lpEventData, dwEventType);
			break;
		}
	//skipping some events
	case SERVICE_CONTROL_INTERROGATE: break; //fall through to send current status
	default: break;
	} //switch

	//Set the status
	ReportServiceStatus(dwSvcStatus, NO_ERROR);
	//returning error status
	return NO_ERROR;
} //ServiceControl


/*
	Purpose: 
		Sets the current service status and reports it to the SCM

	Parameters:
		dwCurrentState - The current state
		dwWin32ExitCode - The system error code
		dwControlsAccepted - The control codes the service accepts and processes in its handler function 
		dwWaitHint - Estimated time for pending operation, in milliseconds

	Return value:
		None
*/
VOID WINAPI ReportServiceStatus(
	DWORD dwCurrentState,
	DWORD dwWin32ExitCode,
	DWORD dwWaitHint,
	DWORD dwControlsAccepted)
{
	/*
		static counter for the check points
		
		Here some info from MSDN

		dwCheckPoint 
		The check-point value the service increments periodically to
		report its progress during a lengthy start, stop, pause, or
		continue operation. For example, the service should increment
		this value as it completes each step of its initialization when
		it is starting up. The user interface program that invoked the
		operation on the service uses this value to track the progress
		of the service during a lengthy operation. This value is not
		valid and should be zero when the service does not have a start,
		stop, pause, or continue operation pending
	*/
	static DWORD dwCheckPoint = 1;

	//Filling in the SERVICE_STATUS structure
	serviceStatus.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
	serviceStatus.dwCurrentState = dwCurrentState;
	serviceStatus.dwWaitHint = dwWaitHint;
	serviceStatus.dwWin32ExitCode = dwWin32ExitCode;
	serviceStatus.dwControlsAccepted = dwControlsAccepted;
	
	/*
		set to default value because there's no need in the usage of
		the specific errors

		Again some text from MSDN
		
		dwServiceSpecificExitCode 
		A service-specific error code that the service returns when an
		error occurs while the service is starting or stopping. This value
		is ignored unless the dwWin32ExitCode member is set to
		ERROR_SERVICE_SPECIFIC_ERROR.
	*/
	serviceStatus.dwServiceSpecificExitCode = SERVICE_RETURN_CODE;

	//Setting the checkpoints
	if ((dwCurrentState == SERVICE_RUNNING) || (dwCurrentState == SERVICE_STOPPED))
	{
		serviceStatus.dwCheckPoint = DEFAULT_CHECKPOINT;
	}
	else
	{
		serviceStatus.dwCheckPoint = dwCheckPoint++;
	}

	//Set the status and report it to the SCM
	SetServiceStatus(serviceStatusHandle, &serviceStatus);
} //ReportServiceStatus