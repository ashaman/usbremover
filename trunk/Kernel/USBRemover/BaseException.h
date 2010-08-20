/*
	BaseException.h
	Started: 19.07.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>
	
	BaseException class: declaration
*/
#ifndef _H_BASEEXCEPTION
#define _H_BASEEXCEPTION

#include <windows.h>

class BaseException
{
private:
	BaseException* innerException;
public:
	BaseException();
	BaseException(BaseException* inner);
	virtual ~BaseException();
	virtual LPTSTR ErrorMessage() const = 0;
};

#endif