USB Remover - free, open source analog of USB Safely Remove

We wanted to do a very useful application for Windows and we noticed that there's no appropriate flash drive remover which is free! So, we want the situation to change. If someone's interested in development, please, mail me (DarthYarius_0990@mail.ru) or my colleague Yury.

14:07 31.10.2009
DarthYarius_0990@mail.ru
The process of development started today. I have started to implement abstract class for device manager.

17:32 03.11.2009
DarthYarius_0990@mail.ru
After some consultations we decided that I should do the device manager and Yury - process manager.
I found a very interesting article for processes and files management. The only difficulty is to translate C++ code to Delphi...

0:35 05.11.2009
Some difficulties occured with getting size and serial number of flash drive. I hope I'll solve 'em in one-two hours tomorrow.
And next, the main problem is to notify all running Windows applications that the flash drive was ejected

20:12 08.12.2009
I found WMI libraries in Delphi 7. To add them, it's necessary to do the following things
	1) install MS SDK 5.0 or higher
	2) import type libraries
		a) Active DS Type Library
		b) Microsoft WMI Scripting (v. 1.2) library
So, now I'll try to use them in my program

11:37 09.12.2009
It is necessary to create Volume class...