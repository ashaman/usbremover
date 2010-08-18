1) Useful MSDN pages

Displaying Volume Paths - how to use GetVolumePathNamesForVolumeName function

(OK! That's what I needed!!!)


2) Richter - Win32 API
Look at SEH - structured exception handling!!!

3)11.08.2010
The following modules are leak-free:
	DeviceManager.cpp
	Device.cpp
	Volume.cpp
	Drive.cpp
	USBDevice.cpp
	Main.cpp

P.S. It works with the simpliest functionality - gets all the information;

4)Two operations can't be performed simultaneously. A bug occured when the 
VolumeManager caused an refresh while the service was searching for opened files.
The bug (as I thing) is in the GetMountPoints - I do not copy mount points, but copy links on them.
What is better - to use a global mutex (when processes are in the search progress),
to use better realization of GetMountPoints method (with refreshing and so on) or to copy strings
into the buffer?

5) Look a MSDN article for QueryDosDevice

0:34 19.08.2010
The question with GetMountPoints for each device was solved. There's no need in mount points for each device.
Maybe later this will be done better than now...