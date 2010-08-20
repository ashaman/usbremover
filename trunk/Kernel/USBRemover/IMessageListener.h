/*
	IMessageListener.h
	Started: 12.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Message listener class declaration
*/
#ifndef _H_IMESGLISTENER
#define _H_IMESGLISTENER

#include <set>

/*
	Description:
		Provides an abstract interface for message listeners
*/
class IMessageListener
{
public:
	virtual void RefreshState() = 0; //refresh the listener's state
};

/*
	Description:
		Provides an abstract interface for volume listeners
*/
class IVolumeListener
{
public:
	virtual void RefreshVolumeState() = 0; //refreshes the volume listener's state
};

/*
	Description:
		Provides an abstract interface for callbacks
*/
class IProgressCallbackListener
{
public:
	virtual void ReportProgress(byte percentage) = 0; //report about the progress
};

//type definition for delegate-like style
typedef std::set<IMessageListener*> DelegateSet;
typedef std::set<IVolumeListener*> VolumeListenerSet;

#endif