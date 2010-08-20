/*
	WinAPIException.cpp
	Started: 23.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>
	
	WinAPIException class: implementation
*/

#include "WinAPIException.h"

WinAPIException::WinAPIException(DWORD errorCode)
{
	this->errorCode = errorCode;
	this->GetMessage();
}

WinAPIException::~WinAPIException()
{
	DELARRAY(message);
}

/*
	Purpose:
		Gets a message for the error code
	Parameters:
		None
	Return value:
		None
*/
void WinAPIException::GetMessage()
{
	this->message = new TCHAR[MAX_PATH+1];
	ZeroMemory(this->message, MAX_PATH);
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
		NULL, this->errorCode, 
		MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL),
		this->message, MAX_PATH, NULL);
}