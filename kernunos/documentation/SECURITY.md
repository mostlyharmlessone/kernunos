# Using kernunos securely

As far as any numerical Fortran code is concerned, inappropriate input to the algorithm may cause an exception and therefore theoretically cause a security breach. Having said that, more effort was made than it might appear to catch errors in input to avoid program crashes or other inappropriate behavior.

While there are some checks of the sanity of the input with error code handling, there are few checks on memory allocation or size of the problem. Concerns about cybersecurity and C/C++ are duly noted, though the author disclaims any competency in understanding or addressing such concerns.  There are certainly a great number of bugs of my own as well.  This program is not designed to be run remotely on an internet facing machine, not does it have any network capability not granted by the underlying operating system I/O, nor are multiple instances supported.  

The project uses some upstream code which may have unknown vulnerabilities, backdoors, trojans, viruses, worms and bugs. sparsekit.f90 for example has at least one missing function







