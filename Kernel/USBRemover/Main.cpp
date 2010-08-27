/*
	USBREMOVER APPLICATION
	VERSION 2.?.?.?
	
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru

	License: LGPL v3 <?>

	Here are the most important points:
	0) 20.05.2010 - start of development
	1) 11.08.2010 - it gets all the information and disposes 
	it carefully. I found no memory leaks
	2) 12.08.2010 - it refreshes dynamically the information
	and disposes it in a system service
	3) 16.08.2010 - opened files search implemented
*/
#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
//_CrtMemState s1, s2, s3;
#endif

#include <stdio.h>
#include <tchar.h>
#include "SrvMain.h"

/*
	APPLICATION ENTRY POINT
	Main function of the service
*/
int _tmain(int argc, _TCHAR* argv[])
{
	/*
		Here we initialize the table of service entry points.
		Each row consists of a service name and a pointer to
		the main service function
	*/

	SERVICE_TABLE_ENTRY dispatcherTable[] = 
	{
		{serviceName,(LPSERVICE_MAIN_FUNCTION)ServiceMain},
		{NULL, NULL}
	};

	/*
		The following lines run the service dispatcher
	*/
	if (!StartServiceCtrlDispatcher(dispatcherTable))
	{
		_tprintf(_T("Start of service failed. Error %ld\n"), GetLastError());
		return GetLastError();
	}

	return 0;
}