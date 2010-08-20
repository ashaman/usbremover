/*
	ManagerException.h
	Started: 19.07.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>
	
	ManagerException class: declaration
*/
#ifndef _H_MANAGEREXCEPTION
#define _H_MANAGEREXCEPTION

#include <windows.h>
#include <tchar.h>

#include "BaseException.h"


/*
	Class which signals about problems in the device manager
*/
class ManagerException: public BaseException
{
private:
	LPTSTR message;
public:
	ManagerException(LPTSTR message);
	~ManagerException();
	LPTSTR ErrorMessage() const {return this->message;};
};

#endif