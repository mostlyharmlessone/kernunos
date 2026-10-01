# Using kernunos securely

As far as any numerical Fortran code is concerned, the weakest points are probably in the I/O routines being fed malformed or unexpected data files causing a crash, i.e."unsanitized input". Having said that, more effort was made than it might appear to catch errors in input to avoid program crashes or other inappropriate behavior.

While there are some checks of the sanity of the input with error code handling, there are few checks on memory allocation or size of the problem. Concerns about cybersecurity and C/C++ are duly noted, though the author disclaims any significant competency in understanding or addressing such concerns.  No doubt there are memory leaks, particularly when data is passed back and forth between Fortran and C++.  There are certainly a great number of bugs of my own as well, as well as poor handling of pointers etc..

There are no intentional exploitable defects, backdoors or malicious code.  Similarly, although the license specifically precludes use of an "AI" to read, use or otherwise interact with this project, there are no hidden injection prompts.  Read the LICENSE.

The use of third party libraries is limited to decrease the chances of unknown vulnerabilities, backdoors, trojans, viruses, worms and bugs, although the use of some outside code is unavoidable and in many cases, desirable.

For example: sparsekit.f90 has at least one missing function, and on any modern compiler issues many, many warnings. It is ancient FORTRAN code, and I have not throughly vetted it.

GLSL code known to be unsupported (and unstable causing a crash such as geometry shaders) on the MacOS/Darwin has been disabled.

If you find any vulnerabilities you feel should be reported, please open an issue detailing the problem with a proof of concept if possible and a solution. Bear in mind that this program is not designed to be run remotely on an internet facing machine, nor as a web app, nor does it have any network capability not granted by the underlying operating system I/O, nor are multiple instances supported.

Read the disclaimers regarding use in the README.







