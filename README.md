# Kernunos

*"The purpose of computing is insight, not numbers"*<br>

Kernunos is a program for analyzing/viewing corneal topography data from different topographers, a technology which became increasingly common in ophthalmology in the 1990's.  Many data files are in an undocumented, non-ASCII/Unicode state, a trend which has unfortunately increased with newer technology. The term "enshittification" has been coined recently, which describes the larger phenomenon.  Unfortunately as well, the standard for interoperability of "data" exchange (ie. DICOM https://www.dicomstandard.org/) is image based. Unless the raw data actually is an image, DICOM is rather like sending text by fax machine (as opposed to sending an ASCII or word processing file). A fax is still the common format for exchanging data between medical offices in the USA. A similar phenomenon entails publishing "data" as a picture in a scientific journal, rather than supplying the points used to make the graph.  It is recognized that the "data" provided by the manufacturers below, when supplied at all, has already been processed from the raw image data through a series of unknown proprietary algorithms with an arbitrary amount of precision being presented. As such we are not much better off than being presented with a graph in a journal, laying some graph paper over it and manually digitizing it.<br>

As has been noted by numerous authors in the field, it is hard to directly compare topography from different machines [Interchangeability_between_Placido_disc_and_Scheim.pdf](file:./docs/Interchangeability_between_Placido_disc_and_Scheim.pdf), making clinical studies as well as patient care more difficult.  The goal of this project is to provide a tool for doing so, (improving intraoperability) particularly to re-analyze older machines output with modern notions despite the limitations of the information we have. There are many older or discontinued models which are still in clinical use despite their technology being at this writing 30-40 years old. It is unfortunate that in only a generation later we are having to turn to software archeology. https://en.wikipedia.org/wiki/Software_archaeology <br>

Each machine typically has and had its own internal analysis software as well as its own unique data storage.  There are newer machines which are also tomographic machines, in that they measure the thickness of the cornea, as well as machines that measure aberrometry of the whole eye. It would be lovely if the whole industry agreed on using an open data format, allowing for independent analysis, or if there were a method for importing the photographic data from one machine to another, along with the geometric data of the image cone.  Newer machines, particularly OCT machines as used in posterior segment (retina and optic nerve) images are now being seen for corneal scanning increasingly as well: e.g. Heidelberg's Anterion, or the latest Oculus Pentacam. In general, there is no publicly available description of the exact data rendered and how it is converted to the pictures we see, making it difficult to even know what to look for in the binary data files, assuming they have not been further intentionally obfuscated. https://en.wikipedia.org/wiki/Obfuscation_(software)


## Color scales and legends

"*Des goûts et des couleurs on ne discute pas... Mais nous ne faisons que cela.*"<br>

A great amount of time was spent in the 1990's debating color scales.   There were numerous presentations regarding how to portray the data, which colors to use, whether red should be on top or bottom of the scale, etc., but little about the quality or nature of the data being presented.  <br>

In general, we can hopefully agree on some terminology: There are absolute color scales, with colors matched to specific values, and relative scales, where a palette is spread over the range of values found in any particular data set. An absolute color scale has a fixed range and constant palette. One perceived advantage to an absolute scale for clinicians is easy identification of values inside normal or outside normal, with a characteristic color pattern for several common diseases without the need for actually looking at the values of the curvatures, or knowing anything about them.  One objective disadvantage to an absolute scale is that small differences in curvatures are lumped into common colors and become invisible.  Relative/non-fixed scales show differences in any particular surface quite well, but interpretation is dependent on reading the values on the legend and comparing them with accepted normals known by the clinician.  Strange as it may seem, there are differences in clinical acceptance and scientific knowledge.<br>

There is an ANSI standard absolute color scale Z80.23 (which is not freely available nor does it specify an exact color palette), as well as eponymous scales from different manufacturers and authors with industry ties. <br>

A Universal Standard Scale was proposed in 2002, which is a modified ANSI scale with a freely available definition.  It is an absolute scale, sometimes referred to as the Smolek-Klyce scale after its creators. There are variations of the scale. The range of the scale was explicity designed for "Corneal Power", a slope based definition also called "Axial or Sagittal Curvature"and was adopted by at least one manufacturer, and designed to be useful for diagnosing clinical disease. See [Curvature_notes.pdf](file:Curvature_notes.pdf)<br>

In addition, there are non-"rainbow" perceptually uniform color scales which take color vision deficiencies into account, none of which (oddly enough) seem to be in use by ophthalmological manufacturers (or are not documented as such) and are not specific to the cornea. There are two points to consider: (1) It is unknown to me whether normal color vision is still a requirement for admission into ophthalmology programs in the US, though it is certainly not usually a requirement for researchers AFAIK.  The market does not seem to have made allowances for this group. (2) When faxing records, pictures are rendered black and white: any chance of transmitting a useful image is greatly decreased with the use of a "rainbow" scale.<br>

To that end, the program allows the user to select a number of different color scales in order to form their own opinion about which serves them best for that particular data set and function being displayed.<br> 


## Pupils

 "*...a tale Told by an idiot, full of sound and fury, Signifying nothing.*"<br>

Most of these corneal topography machines also capture data about the size and location of the subject's pupil.  The clinical importance of the pupil position and size relative to the cornea has been the subject of much heated, sometimes scholarly, discussion and at least one notorious lawsuit in which the leading expert later recanted their testimony after the verdict (which of course did not alter the verdict, nor allow for appeal, AFAIK). Strange as it may seem, there are even greater differences between legal findings than between clinical acceptance and scientific knowledge. This program allows display of the pupil data in conjunction with the corneal data.

## Curvatures, Functions and Zernike

"*You keep using that word. I do not think it means what you think it means." (‘The Princess Bride’ - 1987*) <br>

Optical qualities of the cornea historically were the driver of the interest in curvature, using classic optical theory of lenses.   Unfortunately, what clinicians and mathematicians mean by curvature is not always the same. See also [Curvature_notes.pdf](file:Curvature_notes.pdf)<br>

By convention, curvatures are converted from units of length to their inverse in corneal diopters by a constant, the keratometric index, usually agreed to be 337.50 mm/D in the United States, though 332.0 is more common in Europe, with different devices possibly using 331.5, 333, 336, 338 or 376, with an extensive literature supporting different suggestions. Considering the measurement errors because of axis misalignments, among other things, particularly in the case of Placido measurements where the assumption that reflected rays remain in their meridional planes directly contradicts axial asymmetry, it seems as though a lesson about significant digits might well have been reviewed.<br>

In addition, more recently, interest in finer optical distinctions have driven clinicians and the manufacturers of topographers to incorporate measures of optical aberrations. Surfaces can be represented as functions on a cartesian grid z(x,y) can be represented by the approximations with the familiar Fourier series in Cartesian coordinates, or Bessel functions in polar coordinates.  There are many uses for breaking down the surface shape to (basically) its frequency space as represented by the coefficients which weigh the relative amount of each basis function.  In optics, Zernike functions have historically been popular because they are expressed in polar coordinates, like Bessel functions, but are better known in the microscopy and optics literature. The coefficients of the Zernike functions have some clinical correlates to gross optical properties, such as astigmatism, as well as wavefront optical aberrations, whereas the Bessel functions are better suited to accurately describe the geometry of a surface.  Clinicians typically look at the corneal data with several different representations depending on the situation.  For example, so-called "tangential" curvatures are felt to be better than "axial" curvatures at detecting and distinguishing prior ablation and type of ablation after myopic or hyperopic LASIK or PRK. The Zernike coefficient corresponding to optical coma is felt to be the most symptomatic so-called higher order aberration.  Modern manufacturers of clinical equipment have provided computations of Zernike coefficients (and in one case Fourier) for a given corneal surface. Typically these are based on wavefront aberrometry of the whole eye, not just the anterior corneal surface, but documentation of exactly what is computed and how is lacking. <br>

It is also an increasingly common practice for manufacturers to provide several undocumented proprietary "indices" or numbers that are meant to provide a relatively simple clinical answer regarding important pathological features of the cornea or optics of the whole eye, again highlighting the difference between clinicians and ... everyone else.  No doubt soon, a diagnosis will emanate from the latest notorious oracular blackbox, an LLM.  If that is what you want, I am surprised you managed to read this far.

## Hardware

"*The magnitude of this error is as yet undetermined, although it is thought to be small.*" <br>

The first, and older, class of machines are Placido disk machines, which take a photo of a reflection of rings on the surface and compute the shape based on the distortion of the image, subject to a number of assumptions, one important being an essentially 2-D simplification that along each radial mire reflected rays stay in their plane (which is not the case for non-axisymmetric or decentered axisymmetric shapes).  Machines do not actually use a flat Placido disk, but rather a cone with concentric rings to maximize the coverage area of the cornea by aiming for evenly spaced images of rings on a test sphere. <br>

 [./images/Placido_cone.jpeg](file:./images/Placido_cone.jpeg)

Of course it was also well recognized that any reconstruction of a 3-D reflecting surface (the cornea) by a 2-D image (particularly an image of concentric circles) is a underdetermined problem; i.e.. there is insufficent information and a number of mathematical assumptions have to made regarding the size and distance of the reflecting object. The mathematical approaches to curve reconstruction in a meridian, such as they were revealed, reflect the limitations of computation in the 1980's, and make quite a lot of simplfying assumptions in order to make the computations possible for the technology at that time.  There are also purely hardware issues regarding spacing of rings, image capture, resolution and digitization of the images. Even with optimal arrangements of rings and the givens of anatomy the surface imaged is generally limited to 60% of the total surface. It was nonetheless claimed at the time that the CMS system mentioned below was capable of 30 micron positional accuracy on the corneal surface. (Subject to the "undetermined but thought to be small" error mentioned above undercutting the entire method!) Note also that it has been calculated that a 16 micron local difference in elevation can equal 1 diopter of corneal power - a staggeringly large number by current clinical standards.  A modern machine claims a "resolution of +/- 0.01D, 1 micron
accuracy / precision axial radius +/- 0.03mm altimetric data, +/- 2µm at 4mm".  I should also point out that manufacturer's sales teams have used the words precision and accuracy (and possibly resolution) interchangeably.   <br>

From an engineering point of view, no meaningful error analysis was ever provided by the manufacturers of this class of equipment.  Access to the raw data (in the open software/hardware sense) was never, as far as I can tell, on the table, making an independent analysis impossible.  In the best case scenario for topographers, that would mean having an image file of the corneal rings taken by the camera (the raw data), measuring the physical Placido cone in the lab (more raw data that we would have to collect), and then writing our own algorithm to determine the implied shape of the cornea by the relationship between the image and the object.  That might be easiest with some calibrated reflective target spheres (sometimes provided by the manufacturer for calibration).<br>

Dry ocular surfaces or unstable tear films can produce broken or irregular mires which can generate completely spurious geometry, so much so that some contemporary sales pitches for  these machines are now focussed on dry eye detection rather than topography.<br>

The other and newer class of machine uses scanning technology to measure elevation either with a scanning laser (including OCT) or Scheimpflug photography, which presumably each have their own error of measurements. There is not as much information published about the details of the technology from an engineering point of view, making an independent estimation of those errors hard to know. Organizations providing tear downs of electronics and equipment such as IFixit, IPRG and EDN have not apparently worked on any topographer.  Caveat emptor! <br>

###On data storage and reading files

"*Do not attribute conditions to villainy that simply result from stupidity.*”<br>

It is rare for data to be written to files as IEEE-754 floating point or ASCII/Unicode, for example in the modern era, and the alternative, yet common choice, of a relatively non-intuitive data storage with questionable computational or storage advantage, (a strategy which has often been deprecated for difficulties in code maintenance), points to possible deliberate obfuscation.<br>

Quoting https://en.wikipedia.org/wiki/Raw_image_format<br>
"Providing a detailed and concise description of the content of raw files is highly problematic. ... Several major camera manufacturers, including Nikon, Canon and Sony, encrypt portions of the file in an attempt to prevent third-party tools from accessing them."  <br>

In a different way to prevent third-party tools from accessing files, it has been implied that attempting to read proprietary file formats could be seen as IP infringement: https://cloudcompare.org/forum/viewtopic.php?t=5623  The legality of reverse engineering has been discussed, for example in the US, quoting the following: https://en.wikipedia.org/wiki/Reverse_engineering#Legality<br>

"According to Section 103(f) of the Digital Millennium Copyright Act (17 U.S.C. § 1201 (f)), a person in legal possession of a program may reverse-engineer and circumvent its protection if that is necessary to achieve "interoperability", a term that broadly covers other devices and programs that can interact with it, make use of it, and to use and transfer data to and from it in useful ways. A limited exemption exists that allows the knowledge thus gained to be shared and used for interoperability purposes."<br>

The EU has similar directives, but I am not qualified to understand the distinctions nor the jurisdictions.  Many open source programs have reverse engineered numerous proprietary formats for the purpose of intraoperability (document file formats, file sharing protocols, disk partition schemes etc.), see the wikipedia articles referenced above.
<br>
On more positive notes:<br>

A notable exception to obfuscation in the topography field comes from Oculus, Inc. USA (OCULUS Optikgeräte GmbH, Germany) who appears committed to exporting some human readable data and providing documentation even with their latest OCT model for both the PentaCam and Keratograph machines.<br>

Carl Zeiss Meditec AG, maker of the Zeiss Atlas series topographer, also appear committed to exporting some human readable data, and in fact have improved their stance, as the later 9000 model and software not only have the capability to export data that the earlier 900 model did not, it also will import the 900 data and then export it, acting as a conversion utility.<br>

Bausch & Lomb, maker of the Orbscan had a separately purchasable viewer software which might have allowed export, however it is apparently discontinued. There was at one point some exportable data, see https://iovs.arvojournals.org/article.aspx?articleid=2125323 <br>

##List of machines

Please see [SUPPORTED_HARDWARE.html](file:SUPPORTED_HARDWARE.html) for a list of corneal topography machines with notes on support in kernunos.


## Disclaimer
 
I have no commercial interest or conflict of interest regarding any of the products or technologies discussed in any of these documents, nor have I ever worked for or acted as a consultant for any of the corporations/companies mentioned here. Any opinions are my own and do not represent those of any previous or current employer.


## Usage
 
It is an embarrassment to have to state the following, as it should be understood to be the default:  No material on this site is intended to be a substitute for professional medical advice, diagnosis or treatment, nor is it meant for a corporate workplace.  If you want a secure professional piece of software, please consult a professional developer.  Speaking of which, this is NOT "vibe-coded", nor written by, nor with use of a so-called AI/LLM.  The information presented on the devices above was almost entirely gained by examining the machines externally and their files when possible, or public information on the internet when not, and its accuracy may therefore very well be limited or incorrect. No information was provided by any manufacturer other than what is available to the public and occasional chats with friendly sales reps for some of the manufacturers, with the notable exception of OCULUS Optikgeräte GmbH (Oculus USA) who were generous and kind enough to supply a sample export file and documentation for the Oculus Keratograph, as well as an export file from their latest OCT incorporating PentaCam machine.  Thank you Oculus! No warranty is made whatsoever that this software will function in the way that it is expected and that the results can be relied on to make clinical decisions. See also the LICENSE. Some example files have been included for demonstration purposes with the files redacted as needed to remove protected personal information.  The program is meant as a tool for understanding and improving intraoperability of data, limited by its design and manufacture, as well as by the user.  
 

## Building

###Under Linux 
This software was developed at various times under the Slackware, Ubuntu, Arch, Manjaro distributions of GNU/Linux. Dependencies include Qt6, assimp, lapack, rply, gnuplot, gnuplot-iostream, boost, freetype, unzip, cabextract and superlu. Some routines are adapted from other sources and included, for example excerpts from Hanson & Hopkins (see below). OpenGL is used as the primary graphics API: GLSL 3.30 is needed as a minimum.  Qt appears to be deprecating C++ support in favor of QML: you may need to make sure the proper support is present. This version was built with Qt 6.8.3. You might need to adjust some of the paths in the CMakeLists.txt to build.<br>

There are some patches to Hanson & Hopkins http://www.siam.org/books/ot134 chapters 2,4, & 11 to accomodate superlu versions > 4.3, for Windows compilation and to support CSR sparse matrices.<br>

For some file formats, Fortran Linux system calls {call execute_command_line() } to cabextract and rm (or Expand and del under Wiindows) are used for convenience in reading compressed Windows cabinet files and cleaning up temporary files. <br>

Linux system calls from C++  { system() } are called for starting with a command shell for gnuplot and assorted file ops such as unzip, rm, touch, cp, or Windows calls to cmd, tar, del, type and copy.<br>

If you don't have access to those system calls, you'll have to extract the cabinet files to their uncompressed data files manually and clean up the temporary files manually, which has not been tested, YMMV.<br>

After cloning the source, load submodules with git submodule update --init, then the usual:<br>

cmake ./ <br>

These lines need to be uncommented in CMakeLists.txt
<br>
set(CMAKE_HOST_NAME Linux)
set(CMAKE_SYSTEM UNIX)
<br>
with these
<br>
set(CMAKE_HOST_NAME Windows)
set(CMAKE_SYSTEM Windows)
<br>
commented out.<br>

gcc is used as the default compiler with cmake.  <br>

Building within QtCreator is also possible, but the default of using Ninja does not work with the Fortran dependencies. I had to manually edit .qtcreator/CMakeLists.txt.user directly to replace ninja in the following lines:<br>
-DCMAKE_GENERATOR:STRING=Unix Makefiles<br>
-DCMAKE_MAKE_PROGRAM:STRING=/usr/bin/make<br>

####Under Windows

As an alternative, build under Windows if you have the necessary toolset.  <br>

The precompiled binary and installer were built in a Windows 10 VM. I used MingW/GCC for the C/C++/Fortran compiler and toolchain under Windows.<br> 
https://doc.qt.io/qt-6/windows.html <br>
I was unable to get Visual Studio and the Intel Fortran compiler to work together. In addition, the additional libraries would have to be compiled with the same toolchain.  I built assimp, superlu, glm, freetype, lapack and OpenBLAS with the same toolset using git-bash after downloading them directly from upstream. Again, you might need to adjust paths in CMakeLists.txt in order to build. These lines need to be uncommented in CMakeLists.txt:
<br>
set(CMAKE_HOST_NAME Windows)
set(CMAKE_SYSTEM Windows)
<br>
with these
<br>
set(CMAKE_HOST_NAME Linux)
set(CMAKE_SYSTEM UNIX)
<br>
commented out.<br>

You will need to run winqtdeploy in order to generate the necessary Qt dlls and copy over the plugins.<br>

gnuplot (https://gnuplot.sourceforge.net/) also needs to be installed for some functions.<br>

As noted, OpenGL is used as the primary graphics API: it needs GLSL 3.30, which means the Windows binary will not run in a VM unless it has access to a real graphics card via passthrough or with a software renderer separately installed, eg.<br> https://github.com/pal1000/mesa-dist-win.<br>
If used, install the Core (option 1) and the software renderer (option 7). The program and installer will run under WINE using the Linux distro's GLSL support, although the Linux distro's gnuplot will not be available. YMMV. The binary Windows installer (built with NSIS) includes an option to install gnuplot and the Mesa3D software renderer. CrossOver(TM) works well, although you might need to manually close some cmd.exe windows manually on installation and when using gnuplot.<br>


####Cross-compiling for Windows under Linux

Cross compiling under Linux is not as straightforward as one would like in 2026.  Perhaps it is not the ideal solution; there was a lot more activity >10 years prior, with the option of building in a VM with native tools becoming more common. As of this writing, I haven't managed a complete cross-compile build. Some notes follow:<br>

Under Arch Linux using AUR packages<br>
yay -S mingw-w64-gcc <br>
yay -S mingw-w64-qt6-base mingw-w64-qt6-tools<br>
https://aur.archlinux.org/packages/mingw-w64-qt6-base<br>
https://github.com/Martchus/PKGBUILDs<br>

or<br>
Using MXE for Qt builds:<br>
git clone https://github.com/mxe/mxe.git<br>
make qt6 MXE_TARGETS='x886_64-w64-mingw32.static'<br>

configure Qt to use use the ming64 qmake, compiler and kit under Tools<br>

These lines need to be uncommented in CMakeLists.txt:<br>
<br>
set(CMAKE_HOST_NAME Linux)
set(CMAKE_SYSTEM Windows)
<br>
with these
<br>
set(CMAKE_HOST_NAME Windows)
set(CMAKE_SYSTEM UNIX)
<br>
commented out.<br>

Numerous libraries can be imported from their (mingw64) builds under Windows.<br>

some other references<br>
Qt<br>
https://doc.qt.io/qt-6/cross-compiling-qt.html <br>
https://stackoverflow.com/questions/10934683/how-do-i-configure-qt-for-cross-compilation-from-linux-to-windows-target <br>
in general<br>
https://github.com/Zeranoe/mingw-w64-build <br>
https://wiki.archlinux.org/title/MinGW_package_guidelines <br>
wxWidgets<br>
https://wiki.wxwidgets.org/Cross-Compiling_Under_Linux <br>


####For MacOS

Under construction.  Best advice seems to be build it on a Mac with the native toolchain.  Since the MacOS is based on BSD and has a similar toolset... possibly use the same instructions as for Linux with minimal changes. It appears the little activity in cross-compiling under Linux has largely been abandoned, possibly because of changes in Apple hardware over the last 10 years, which would not only require cross-compliling for Darwin/MacOS/Quartz, but also for the M1/M2/M3/M.. chips and other Apple only hardware. <br>
https://doc.qt.io/qt-6/macos.html <br>
https://stackoverflow.com/questions/693952/how-to-compile-for-os-x-in-linux-or-windows <br>
https://stackoverflow.com/questions/4342047/compiling-a-qt-application-for-mac-os-x-on-linux <br>


## Contributing

Requests are welcome. For major changes, please open an issue first to discuss what you would like to change. Any errors found with corrections are very welcome, but if you use an AI/LLM please take ownership of the purported issue regardless of the tools you use. Any legally obtained information on different machines and their data storage and/or better yet data files or documentation is welcome and I'll do my best to incorporate it.  If you are a clinician with an older machine and you want to send patient data please XXXX out/edit out patient information from the files or use John/Jane Doe/Test patient. Do not include any protected information.<br>

If you are a manufacturer and you'd like to blow my mind by openly sharing information, I will credit you prominently here and everywhere and sing your praises to all of my colleagues, even at the risk of irritating them. <br>

For example:<br>

OCULUS Optikgeräte GmbH/Oculus USA, your representatives have impressed me (which is actually more difficult than it should be) by their ability to remember me from one meeting to an another years later, as well as their actual follow through and the provision of valuable data.  Your company is also outstanding in your continued committment to providing data for research via export through your software though the raw data would have been even better. Your machine's inherent software is beautiful and well designed. +50 points! Thank you.<br>

On the other hand:<br>

Any changes by a manufacturer that results in obscuring the data further and/or removing export features...well, shame on you, that is not contributing to knowledge or patient care and you are not friends of Narnia imo.  You know who you are.<br>  

Do let me know if you find this software useful, or at least, amusing. Any constructive criticism is welcome; bear in mind I am not a professional coder or programmer.<br>
 

## License
To quote or adapt without proper attribution would be bad manners, to take credit for other's works, dishonest, quite aside from legalities.  I am not a lawyer, nor can really understand, let alone agree with, their worldview despite decades of adult life, starting with "ignorantia juris non excusat". Having said that, in so far as I understand from perusing the multiple versions of licenses for the software used in this project, the source code I have written/copied and adapted conforms to their respective licenses and allows for non-commercial use and redistribution with the caveat that the licenses are included and/or referenced and credit is given when known, which I have in good faith attempted. On that note, no AI/LLM was used for any part of this project, the goal of which has been to exercise my imagination, not to outsource the effort of making things up nor using the information of dubious provenance gathered by an LLM without permission or attribution.  Any use of and examination of proprietary trademarks and data has been, to my understanding for the purpose of this project, to be lawful under applicable laws.  My contributions, including the patches for superlu, and any other adaptations of existing software, are licensed as follows:<br>
[LICENSE](https://github.com/mostlyharmlessone/kernunos/blob/main/LICENSE)<br> if not superseded by the relevant licenses of the adapted software collected under ./licenses.  Written documentation including this README © 1999 by Anthony M de Beus is licensed under CC BY-SA 4.0. https://creativecommons.org/licenses/by-sa/4.0/ 


##Web background references
Broken links, books out of print, and any other disappointments are part of life.

####Wikipedia/Mathematics
There's nothing wrong with using the encyclopedia as a starting point, and like a dictionary used in a popular crossword word making game, at least we can choose to agree on a common reference.<br>
https://en.wikipedia.org/wiki/Bicubic_interpolation <br>
https://en.wikipedia.org/wiki/Differential_geometry_of_surfaces <br>
https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm <br>
https://en.wikipedia.org/wiki/Newton%27s_method <br>
https://en.wikipedia.org/wiki/Gradient <br>
https://en.wikipedia.org/wiki/Polyharmonic_spline <br>
https://en.wikipedia.org/wiki/Zernike_polynomials <br>
https://en.wikipedia.org/wiki/HSL_and_HSV#Color_conversion_formulae <br>
https://en.wikipedia.org/wiki/X11_color_names <br>
https://en.wikipedia.org/wiki/Scheimpflug_principle <br>
https://en.wikipedia.org/wiki/Sparse_matrix <br>
https://en.wikipedia.org/wiki/Tensor <br>

#### openGL copied from/adapted/referenced 
Yeah, well, it was popular when I started the project, and is still widely supported, like C, C++ and Fortran.. which were also still popular when I started. <br>
https://www.khronos.org/opengl/wiki/Getting_Started#Writing_an_OpenGL_Application <br>
https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal <br>
https://open.gl/ <br>
https://openglbook.com <br>
https://www.khronos.org/opengl/wiki/Geometry_Shader <br>
https://www.khronos.org/opengl/wiki/Geometry_Shader_Examples <br>
https://stackoverflow.com/questions/18510701/glsl-how-to-show-normals-with-geometry-shader?rq=3 <br>
https://en.wikibooks.org/wiki/Category:Book:OpenGL_Programming <br>
https://learnopengl.com/ <br>
https://github.com/JoeyDeVries/LearnOpenGL    CCBY-NC-4.0 <br>
https://learnopengl.com                       CCBY-NC-4.0  <br>                           
http://www.opengl-tutorial.org/               CC-BY-NC-ND  / WTF <br> 
https://open.gl/content/code/c2_triangle_elements.txt from https://open.gl/drawing <br>
https://learnopengl.com/Getting-started/Shaders <br>
https://learnopengl.com/In-Practice/Text-Rendering <br>
https://github.com/openglbook/openglbook-samples  MIT <br> 
https://gitlab.com/wikibooks-opengl/modern-tutorials/-/blob/master/text01_intro/text.cpp?ref_type=heads <br>
https://ogldev.org/www/tutorial22/tutorial22.html       ?BSD <br>
https://computingonplains.wordpress.com/opengl-graphics-and-cpp/ <br>
https://github.com/capnramses/antons_opengl_tutorials_book <br>
https://en.wikibooks.org/wiki/Category:Book:OpenGL_Programming <br>
https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal <br>

####Qt copied/adapted from
I did some asking around about a good environment for building a program that could conceivably be compiled for multiple OS/architectures..Qt kept coming up.<br>
https://www.qt.io <br>
qt6/examples <br>
https://github.com/QtOpenGL/qgl_tutorials <br> https://bogotobogo.com/Qt/Qt5_OpenGL_QGLWidget.php <br>
https://www.qt.io/ particularly assistant & opengl examples <br>
https://doc.qt.io/qt-6/qopenglwidget.html <br>

###wxWidgets
used in development, abandoned for Qt at this point <br>
https://www.wxwidgets.org <br>
https://github.com/wxWidgets/wxWidgets/tree/master/samples/opengl <br>
https://github.com/wxWidgets/wxWidgets/tree/master/samples/opengl/pyramid <br>
update glew for wxWidget >= 3.1.5 for egl support <br>
https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=glew-egl-glx <br>
https://stackoverflow.com/questions/13659853/async-execution-with-wxwidgets <br>
wxExecute(_("bash --login -i"), wxEXEC_ASYNC);


###Books

Splines and Variational Methods 1975 PM Prenter, Dover Publications <br>
Numerical recipes in C 1988 WH Press, BP Flannery, SA Teukolsky, WT Vetterling Cambridge University Press. <br>
Introduction to Applied Mathematics 1986 G Strang, Wellesley-Cambridge Press <br>
Computer Methods for Mathematical Computations 1977 GE Forsythe,MA Malcolm, CB Moler., Prentice-Hall  <br>
Numerical Computing with Modern Fortran 2013 Richard J.Hanson and Tim Hopkins SIAM. <br>
Corneal Topography, Measuring and Modifying the Cornea, DJ Schanzlin, JB Robin (editors) 1992 Springer-Verlag <br>


###Journal articles/downloaded class notes/documentation
(in no particular order or format, sourced manually from the internet)<br>

Eric Albin, Ronnie Knikker, Shihe Xin, Christian Oliver Paschereit, Yves d’Angelo. Computational assessment of curvatures and principal directions of implicit surfaces from 3D scalar data. Lecture Notes in Computer Science, 2017, Mathematical Methods for Curves and Surfaces, 10521, pp.1-22. ⟨10.1007/978-3-319-67885-6_1⟩. ⟨hal-01486547⟩<br>

Interchangeability between Placido disc and Scheimpflug system:
quantitative and qualitative analysis (Permutabilidade entre o disco de Plácido e o sistema Scheimpflug: análise quantitativa e qualitativa) VINÍCIUS SILBIGER DE STEFANO1, LUIZ ALBERTO SOARES MELO JUNIOR2, FELIPE MALLMANN2, PAULO SCHOR2  Arq Bras Oftalmol. 2010;73(4):363-6<br>

Corneal topography and the Hirschberg test Scott E. Brodie,APP OPTIC 31(19):3627-3631 (1992)<br>

The Universal Standard Scale Proposed Improvements to the American National
Standards Institute (ANSI) Scale for Corneal Topography
Michael K. Smolek, PhD, Stephen D. Klyce, PhD Jeffery K. Hovis, OD, PhD, Ophthalmology 2002;109:361–369<br>

Versions of the spline programs of de Boor and Rice are available in the program library
of IMSL as ICSFKU and ICSVKU: Least Squares Cubic Spline Approximation I — Fixed Knots
Carl de Boor and John R. Rice General Motors Research Laboratories. The final stages were partially supported by NSF grant GP-7163 and NSF grant GP-4052.<br>

Tridiagonal Matrices: Thomas Algorithm W. T. Lee MS6021, Scientific Computation, University of Limerick<br>

Thin-Plate Splines David Eberly, Geometric Tools, Redmond WA 98052
https://www.geometrictools.com/<br>

APPROXIMATION OF A THIN PLATE SPLINE SMOOTHER USING
CONTINUOUS PIECEWISE POLYNOMIAL FUNCTIONS STEPHEN ROBERTS, MARKUS HEGLAND, AND IRFAN ALTAS SIAM Journal on Numerical Analysis · February 2003<br>

The Least-squares Fitting of Cubic Spline Surfaces to General Data Sets† J. G. HAYES, J. HALLIDAY Author Notes IMA Journal of Applied Mathematics, Volume 14, Issue 1, August 1974, Pages 89–103  <br>

C. de Boor and J.R. Rice, Least squares cubic spline approximation I – fixed knots, Technical Report CSD-TR 20, Computer Sciences, Purdue University (1968). Also available at ftp://ftp.cs.wisc.edu/Approx/tr20.pdf <br>

Least squares cubic splines without B-splines S.K. Lucas
School of Mathematics and Statistics, University of South Australia, Mawson Lakes SA 5095
e-mail: stephen.lucas@unisa.edu.au Submitted to the Gazette of the Australian Mathematical Society May 2003, Accepted July 2003 <br>

Least squares fit to discrete data by a C-2 cubic spline, DC2FIT.f in math77 library
Copyright (c) 1996 California Institute of Technology, Pasadena, CA. Based on Government Sponsored Research NAS7-03001. Algorithm and program designed by C.L.Lawson and R.J.Hanson 'SOLVING LEAST SQUARES PROBLEMS', by Lawson and Hanson, Prentice-Hall, 1974. Programming and later changes and corrections by Lawson,Hanson,T.Lang, and D.Campbell, Sept 1968, Nov 1969, and Aug 1970 Modified 1968 Sept 17 to provide C-2 continuity <br>

The misuse of colour in science communication Fabio Crameri , Grace E. Shephard  & Philip J. Heron NATURE COMMUNICATIONS | (2020) 11:5444 www.nature.com/naturecommunications<br>

Perspectives on corneal topography: a review of videokeratoscopy
Peter R Keller BAppSc(0ptom) Paul P van Saarloos PhD
Centre for Ophthalmology and Visual Science, Lions Eye Institute Clinical and Experimental Optometry 80.1 January-February 1997<br>

Oculus Pentacam Interpretation Guide 3rd edition and Oculus Pentacam Instruction Manual DICOM CONFORMANCE STATEMENT for Pentacam 1.17r64 (2010-05-21), OCULUS Optikgeräte GmbH/Oculus USA<br>

NIDEK Refractive Power/Corneal Analyzer/OPD-Scan III Operator's Manual, NIDEK Co. LTD Feb 2011.<br>

Md. Mamun-Ur-Rashid Khan, M. R. Hossain, Selina Parvin. Numerical Integration Schemes for Unequal Data Spacing.American Journal of Applied Mathematics. Vol. 5, No. 2, 2017, pp. 48-56. doi: 10.11648/j.ajam.20170502.12<br>

Gurnani B, Kaur K. iTrace aberrometry -Identifying occult imperfections in the visual system. Kerala J Ophthalmol 2021;33:373-83.<br>

Axial and Instantaneous Power Conversion in Corneal Topography
Stanley A. Klein and Robert B. Mandel Invest Ophthalmol Vis Sci. 1995; 36:2155-
2159.<br>

Differentiation and Numerical Integral of the Cubic Spline Interpolation
Shang Gao School of Computer Science and Technology, Jiangsu University of Science and Technology, Zaiyue Zhang and Cungen Cao Key Laboratory of Intelligent Information Processing, Institute of Computing Technology, Chinese Academy of Sciences JOURNAL OF COMPUTERS, VOL. 6, NO. 10, OCTOBER 2011<br>

APPLICATION OF B-SPLINE METHOD IN SURFACE FITTING PROBLEM
Fateme Esmaeili *, AliReza Amiri-Simkooei, Vahab Nafisi, Amin Alizadeh Naeini The International Archives of the Photogrammetry, Remote Sensing and Spatial Information Sciences, Volume XLII-4/W18, 2019 GeoSpatial Conference 2019 – Joint Conferences of SMPR and GI Research, 12–14 October 2019, Karaj, Iran<br>


##resources/tutorials
https://github.com/scivision/fortran2018-examples                       MIT <br>
http://www.netlib.org/lapack-dev/lapack-coding/program-style.html <br>
http://www.pdas.com/fmmdownload.html Fortran 90 versions of Computer Methods for Mathematical Computations Forsythe et al. 1977 subroutines (Public Domain) <br>
https://github.com/certik/fortran-utils Copyright (c) 2012 Ondřej Čertík   MIT <br>
http://www.siam.org/books/ot134 Numerical Computing with Modern Fortran Richard J.Hanson and Tim Hopkins SIAM some of their routines are copied or adapted explicitly (copyright) <br> 
https://amytabb.com/tips/2022/02/27/least-squares-with-equality-constraints/ <br>
https://web.stanford.edu/class/me200c/tutorial_90/08_subprograms.html factorial (copyright Stanford University) <br>
https://www.mathworks.com/company/newsletters/articles/analyzing-lasik-optical-data-using-zernike-functions.html <br>

###enshittification
trying to avoid it, I have downloaded the PDF of the forbrukerradet (fuggedaboutit?)report for your reading pleasure in case the link gets broken in the future <br>
https://www.theregister.com/2026/03/06/forbrukerradet_aim_enshittification/ <br>
related: https://en.wikipedia.org/wiki/Software_archaeology <br>
https://en.wikipedia.org/wiki/Reverse_engineering#Legality <br>

###gnuplot incorporation stuff
https://stackoverflow.com/questions/28892434/how-to-plot-a-graph-using-gnuplot-from-c-program <br>
https://code.google.com/p/gnuplot-cpp/source/browse/trunk/gnuplot_i.hpp <br>
https://github.com/dstahlke/gnuplot-iostream <br>
http://stahlke.org/dan/gnuplot-iostream/ <br>
https://stackoverflow.com/questions/62848395/horizontal-bar-chart-in-gnuplot <br>

###graphics tools used in development
https://www.meshlab.net/ <br>
P. Cignoni, M. Callieri, M. Corsini, M. Dellepiane, F. Ganovelli, G. Ranzuglia
MeshLab: an Open-Source Mesh Processing Tool, Sixth Eurographics Italian Chapter Conference, page 129-136, 2008 <br>
http://www.gnuplot.info  <br>
GnuPlot in Action, Understanding Data with Graphs, Phillip K Janert, Manning Publications 2010 <br>
libigl: https://libigl.github.io/ <br>

###programming bits & pieces copied/adapted from
https://stackoverflow.com/questions/72566680/count-occurrences-of-character-in-file-c <br>
https://stackoverflow.com/questions/2125880/convert-float-to-stdstring-in-c <br>
https://stackoverflow.com/questions/10750057/how-do-i-print-out-the-contents-of-a-vector <br>
https://community.intel.com/t5/Intel-Fortran-Compiler/How-to-access-ALLOCATABLE-4-D-Fortran-array-from-C/m-p/1478884/highlight/true <br>
https://stackoverflow.com/questions/3418231/replace-part-of-a-string-with-another-string <br>
https://stackoverflow.com/questions/59234281/save-opengl-rendering-to-an-image-file <br>
https://www.modernescpp.com/index.php/asynchronous-callable-wrappers <br>
https://community.intel.com/t5/Intel-Fortran-Compiler/Trouble-reading-a-csv-file/m-p/1034136 <br>
https://fortran-lang.discourse.group/t/joining-strings-problem-with-gfortran/492 <br>
https://stackoverflow.com/questions/54658674/gnuplot-apply-colornames-from-datafile/54659829#54659829 <br>
https://stackoverflow.com/questions/20792445/calculate-rgb-value-for-a-range-of-values-to-create-heat-map <br>
https://stackoverflow.com/questions/3708307/how-to-initialize-two-dimensional-arrays-in-fortran <br>
https://stackoverflow.com/questions/58938347/how-do-i-replace-a-character-in-the-string-with-another-charater-in-fortran <br>
https://fortran-lang.discourse.group/t/how-to-write-bytes-in-a-binary-file/763/7 <br>
https://stackoverflow.com/questions/41254019/reading-variable-length-data-in-fortran <br>

###io.f90 copied/adapted from
https://riptutorial.com/ebook/fortran in particular I/O routine from Chapter 7 I/O:  AL-P, Ed Smith, francescalus, Kyle Kanos, TTT

### color.f90 modified from
https://fortranwiki.org/fortran/show/M_color <br>
http://www.urbanjost.altervista.org/LIBRARY/libGPF/Color/srcf/M_color.HTML <br>
https://colorbrewer2.org <br>
http://www.andrewnoske.com/wiki/Code_-_heatmaps_and_color_gradients <br>
see also for c++ <br>
https://gist.github.com/fairlight1337/4935ae72bcbcc1ba5c72#file-hsvrgb-cpp <br>

###libraries
These are necessary for building/running, under Linux it might be easier/better to use your distro's package manager, if possible, but they can be downloaded from upstream and built as well, which might give better control over the version and ensure you have everything you need. Under Windows, obviously, you need to download and build them yourself.<br>
https://github.com/assimp/assimp <br>
https://github.com/xiaoyeli/superlu  superlu <br>
https://github.com/g-truc/glm glm <br>
https://gitlab.freedesktop.org/freetype/freetype freetype <br>
https://github.com/Reference-LAPACK/lapack lapack <br>
https://github.com/OpenMathLib/OpenBLAS OpenBLAS <br>
https://github.com/boostorg/boost boost <br>
https://github.com/kyz/libmspack/tree/master/cabextract (not needed for Windows)<br>
Only part of this was used, and is copied into the source tree: <br>
https://people.math.sc.edu/Burkardt/f_src/sparsekit/sparsekit.f90 <br>
some of these were used only in development or are references <br>
http://www.netlib.org/lapack/      BSD <br>
http://www.netlib.org/blas/        BSD <br>
https://github.com/jacobwilliams/math77 CalTech license <br>
https://portal.nersc.gov/project/sparse/strumpack/master/GPU_Support.html <br>
https://portal.nersc.gov/project/sparse/strumpack/master/ <br>
https://stdlib.fortran-lang.org/index.html COO/CSR types   MIT<br>
https://stdlib.fortran-lang.org/module/stdlib_sparse_kinds.html   MIT<br>


###cmake 
Some of the find_package() scripts in cmake have been downloaded from here and there, for example:<br>
SuperLU cmake copied from Eigen/libigl <br>
https://github.com/libigl/eigen/blob/master/cmake/FindSuperLU.cmake  MPL2/LPGL <br>
GLM cmake <br>
https://github.com/Groovounet/glm/blob/master/util/FindGLM.cmake MIT <br>
GLFW.cmake <br>
https://gitlab.kitware.com/vtk/vtk-m/blob/783867eeb05e0a6538f9c520af02c3615651b4ed/CMake/FindGLFW.cmake <br>
I also tried CPM, but couldn't figure out how to resolve dependencies such as installing BLAS before LAPACK and both before SuperLU in a single CMakeLists.txt, for example:<br>
https://github.com/cpm-cmake/CPM.cmake CPM.cmake <br>


###viennaCL
not currently used, but with an eye towards possible OpenCL in the future<br>
http://sourceforge.net/projects/viennacl/files/1.7.x/ViennaCL-1.7.1.tar.gz/download  MIT

###rplycpp 
Convert ASCII PLY to binary PLY; MIT licence <br>
https://w3.impa.br/~diego/software/rply/ <br>
https://github.com/diegonehab/rply <br>

##licenses
Sources cited above which have no explicit licenses such as sparsekit.f90 are assumed to be public domain. Credit is preserved in the source code and here to the original authors when known. The following licenses apply to the respective software that utilizes them:
https://github.com/non-ai-licenses/non-ai-licenses/blob/main/NON-AI-MIT <br>
GPL.v3 https://github.com/libigl/libigl/blob/main/LICENSE.GPL <br>
LGPL https://github.com/libigl/eigen/blob/master/COPYING.LGPL <br>
MPL2  https://www.mozilla.org/en-US/MPL/2.0/ <br>
CC B-NC 4.0 https://creativecommons.org/licenses/by-nc/4.0/legalcode <br>
CC-BY-NC-ND https://creativecommons.org/licenses/by-nc-nd/3.0/fr/deed.en <br>
WTF Public License http://www.opengl-tutorial.org/download/ <br>
MIT https://opensource.org/licenses/MIT <br>
BSD (modified) http://www.netlib.org/lapack/LICENSE.txt <br>
superlu library https://github.com/xiaoyeli/superlu/blob/master/License.txt <br>
patches to Hanson & Hopkins http://www.siam.org/books/ot134 chapters 2,4, & 11 (eg to accomodate superlu versions > 4.3, to support CSR sparse matrices and Windows compilation), see above under [LICENSE], all original software from Numerical Computing with Modern Fortran 2013 Richard J.Hanson and Tim Hopkins is copywrited by SIAM and stated to be freely offered without restriction other than crediting the authors, the book and SIAM <br>
patches to sparsekit.90 and sparsekit_test01.f90, see above under [LICENSE] original software from https://people.math.sc.edu/Burkardt/f_src/sparsekit/sparsekit.html  <br>
wxWidgets https://github.com/wxWidgets/wxWidgets/blob/master/docs/licence.txt <br>
Qt https://doc.qt.io/qt-6/licensing.html (LGPL & GPL) <br>

##file formats
here's that encyclopedia again <br>
https://en.wikipedia.org/wiki/STL_(file_format) <br>
https://en.wikipedia.org/wiki/OFF_(file_format) <br>
https://en.wikipedia.org/wiki/PLY_(file_format) <br>
https://en.wikipedia.org/wiki/Truevision_TGA <br>
https://en.wikipedia.org/wiki/TIFF <br>

##assimp
https://assimp-docs.readthedocs.io/en/v5.3.0/ <br>
https://github.com/assimp/assimp <br>
https://github.com/assimp/assimp/issues/3827 <br>

##trademarks
No proprietary information was provided by any of the holders of the following trademarks, other than as previously noted. Trademarks are presented by way of explanation and education only and remain the property of their holders, a list follows but may not be so much exhaustive as exhausting..<br>
EyeSys technologies CAS Corneal Analysis System undocumented file format in ASCII RA*.* and XX*., PU*., EY*., PA*., HX*.<br>
Atlas Zeiss undocumented proprietary format in binary: embedded Embarcadero database .ib;  undocumented file format ASCII .CSV "Export for Research"<br>
Oculus PentaCam undocumented binary .U12; file format for export in ASCII .ELE and .CUR <br>
Oculus Keratograph undocumented file format binary .U12, all files CORNEA(F).O*, CURVAT(F).O*, PUPIL.O*, CENTER.O*,EXP_Topo_O*.zip, PATIENT.TXT, EXAM.TXT, ZERNIKE.CSV<br>
Bausch and Lomb OrbScan<br>
Marco/Nidek OPD-Scan III Refractive Power/Corneal Analyzer undocumented ASCII and binary .CAB versions of RA*.* and ED*., PR*., EY*., PE*., HT*.<br>
Topcon CA-800, undocumented data files, compressed as .lzh <br>
iTrace/Alcon/Tracey Technologies, indices DLI "dysfunction lens index", CPI "corneal performance" and QVI "quality of vision", undocumented data files compressed as zip<br>
Microsoft Excel, "cabinet" files<br>
Phorcides Contour Engine https://phorcides.com/ coined the term "Talus maps" see also
https://support.phorcides.com/installation/<br>
Heidelberg Engineering "Anterion" <br>
VisCam/Solidworks, Materials Magic<br>
Slackware, Ubuntu, Arch, Manjaro, GNU/Linux, LAPACK, gnuplot, Qt, Windows, MacOS<br>
Cultural references are made with utter respect for their creators, CS Lewis, Shakespeare, everyone associated with The Princess Bride.


