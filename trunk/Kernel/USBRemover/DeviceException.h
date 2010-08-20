#ifndef _H_DEVICEEXCEPTION
#define _H_DEVICEEXCEPTION

#include <windows.h>
#include <tchar.h>

#include "BaseException.h"

class DeviceException: public BaseException
{
private:
	LPTSTR message;
public:
	DeviceException(const LPTSTR message);
	~DeviceException();

};

#endif