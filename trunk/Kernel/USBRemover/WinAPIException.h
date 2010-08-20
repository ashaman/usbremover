/*
	WinAPIException.h
	Started: 23.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>
	
	WinAPIException class: declaration
*/

#ifndef _H_WINAPIEXCEPTION
#define _H_WINAPIEXCEPTION

#include <windows.h>

#include "ApiWrapper.h"

#include "BaseException.h"

/*
	Class which wraps the Windows API error codes.
	Translates codes to the error messages
	Inherited from BaseException
*/
class WinAPIException: public BaseException
{
private:
	DWORD errorCode;
	LPTSTR message;
	void GetMessage();
public:
	WinAPIException(DWORD errorCode); //constructor
	~WinAPIException(); //destructor
	LPTSTR ErrorMessage() const {return this->message;}; //error message
	DWORD ErrorCode() const {return this->errorCode;}; //error code
};

#endif