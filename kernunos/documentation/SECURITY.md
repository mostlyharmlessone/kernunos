# Using kernunos securely

As far as any numerical Fortran code is concerned, the weakest points are probably in the I/O routines being fed malformed or unexpected data files causing a crash, i.e."unsanitized input". Having said that, more effort was made than it might appear to catch errors in input to avoid program crashes or other inappropriate behavior.

While there are some checks of the sanity of the input with error code handling, there are few checks on memory allocation or size of the problem. Concerns about cybersecurity and C/C++ are duly noted, though the author disclaims any competency in understanding or addressing such concerns.  No doubt there are memory leaks, particularly when data is passed back and forth between Fortran and C++.  There are certainly a great number of bugs of my own as well.  This program is not designed to be run remotely on an internet facing machine, not does it have any network capability not granted by the underlying operating system I/O, nor are multiple instances supported.

There are no intentional exploitable defects, backdoors or malicious code, but if there were, why would you believe me telling you there aren't?   Similarly, although the license specifically precludes use of an "AI" to read, use or otherwise interact with this project, there are no hidden injection prompts.  Read the LICENSE.

One of the advantages of limiting or not using third party libraries, such as a very large general purpose code base with a complicated API, imho, is that at least I don't have to worry about someone else's intentional or unintentional vulnerabilities.

However, the project unavoidably does use some upstream code which may have unknown vulnerabilities, backdoors, trojans, viruses, worms and bugs. sparsekit.f90 for example has at least one missing function.







