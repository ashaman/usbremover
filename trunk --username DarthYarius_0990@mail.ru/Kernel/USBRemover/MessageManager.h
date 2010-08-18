/*
	MessageManager.h
	Started: 12.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Message manager class: declarations
*/

#ifndef _H_MSGMGR
#define _H_MSGMGR

//API headers
#include <windows.h>
#include <dbt.h>


#include "ApiWrapper.h"
#include "WinAPIException.h"
#include "IMessageListener.h"

/*
	Description:
		This class is for registering and handling device arrival and
		removal notifications. It is designed in such way that it is
		possible to use both service status handles and window handles
*/
class MessageManager
{
private:
	bool pause; //paused or not
	HANDLE wndHandle; //receiver window handle
	HDEVNOTIFY notificationHandle; //device notification handle
	PDEV_BROADCAST_DEVICEINTERFACE messageFilter; //message filter for the devices;
	DelegateSet *listeners; //listeners

	void FireEvent(); //notifies all listeners abouth the changes
	void SetMessageFilter(); //setting message filter
	friend DWORD WINAPI InvokeOnInstallation(LPVOID parameters); //invocated on install event
public:
	MessageManager(HANDLE hWnd, DWORD flags); //constructor
	~MessageManager(); //destructor
	void AttachListener(IMessageListener &listener); //registering message listener
	void DetachListener(IMessageListener &listener); //unregistering message listener
	void Pause(); //pauses the manager's work
	void ProcessEvent(LPVOID messageData, DWORD messageCode); //processing messages by their code
	void Resume(); //resumes the manager's work
};

#endif