/*
	USBDevice.h
	Started: 18.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>
	
	USBDevice class: declarations
*/

#ifndef _H_USBDEVICE
#define _H_USBDEVICE

#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
#endif

//Windows & SDK headers
#include "ApiWrapper.h"

//STD & classes headers
#include "Device.h"

/*
	USBDevice class
	Purpose:
		Represents a USB stick itself
*/
class USBDevice: public Device
{
public:
	USBDevice(HANDLE instanceHandle, HANDLE informationSet);
	~USBDevice();
	bool Eject();
};

#endif