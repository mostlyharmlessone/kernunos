/****************************************************************************
**
** Copyright (C) 2024 Anthony M de Beus
**
** This file uses parts of the examples of the Qt Toolkit or is adapted therefrom.
**** Contact: https://www.qt.io/licensing/
****
** $QT_BEGIN_LICENSE:BSD$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** BSD License Usage
** Alternatively, you may use this file under the terms of the BSD license
** as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of The Qt Company Ltd nor the names of its
**     contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "kernunos.h"

//https://community.intel.com/t5/Intel-Fortran-Compiler/How-to-access-ALLOCATABLE-4-D-Fortran-array-from-C/m-p/1478884/highlight/true
// seems like common blocks were much easier
//not found for some reason, might need explicit location here
//#include "/usr/lib/gcc/x86_64-pc-linux-gnu/13.2.1/include/ISO_Fortran_binding.h"

#include <QtWidgets>
#include <QtConcurrent>
#include <QSlider>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QPushButton>
#include <QApplication>
#include <QMessageBox>
#include <QMainWindow>
#include <iostream>
#include <QTemporaryFile>
#include <QFont>

#include <assimp/cimport.h>
#include <assimp/Importer.hpp>
#include <assimp/Exporter.hpp>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

#include "assistant.h"
#include "gnuplot-iostream/gnuplot-iostream.h"

#include "../kernunos/counter.h"
#include "../kernunos/logc.h"


using namespace QtConcurrent;

// global settings
const unsigned int SCR_WIDTH = 1800;
const unsigned int SCR_HEIGHT = 600;

// flag xxxxxxxx dat,fct,map,action used to communicate between cpp and fortran code calculation options
// first two digits are Placido disk data fillin and/or center-node tweaks
// dat = first binary bit 0/1 centernode tweak
// dat = second binary bit 0/1 shift r-values tweak
// dat = third binary bit 0/1 cubic spline integration (=1) vs trapezoidal rule (default = 0) integration of slopes for elevation
// dat = fourth binary bit 0/1 fillin2 cannot be combined with splinefillin
// dat = fifth binary bit 0/1 splinefillin cannot be combined with lsqfillin
// dat = sixth binary bit 0/1 decenter tweak
// dat = seventh binary bit 0/1 pupilregister tweak
// dat = eighth binary bit 0/1 atlas spline consistency check tweak
// dat = ninth binary bit 0/1 lsq instead of circumferential spline tweak
// dat = tenth binary bit 0/1 2d vs 1d x 1d spline tweak
// dat = eleventh binary bit 0/1 axisymmetric tweak
// second two digits are the function to be plotted as colors
// 0 = SAGC Sagittal or Axial power
// 1-15 = zernike coefficient maps
/*
    fct   HOA
    15    "Z(4,4) Vertical Quatrafoil",  Quadrafoil 0 deg
    13    "Z(4,2) Vertical 2nd Astig.",  4th order astigmatism 0 deg
    09    "Z(4,0) Spherical Aberration", Spherical Aberration
    04    "Z(4,-2) Oblique 2nd Astig.",  4th order astigmatism 45 deg
    01    "Z(4,-4) Oblique Quatrafoil",  Quadrafoil 22.5 deg
    14    "Z(3,3) Oblique Trefoil",      Trefoil 0 deg
    11    "Z(3,1) Horizontal Coma",      Coma 0 deg
    06    "Z(3,-1) Vertical Coma",       Coma 90 deg
    02    "Z(3,-3) Vertical Trefoil",    Trefoil 30 deg
          LOA
    12    "Z(2,2) Vertical Astig.",      Astigmatism 0 deg
    08    "Z(2,0) Defocus",              Defocus
    03    "Z(2,-2) Oblique Astigmatism", Astigmatism 45 deg

    05     Z(1,-1) Y tilt                Y tilt
    10     Z(1,1)  X tilt                X tilt
    07     Z(0,0)  Piston                Height
 */
// 16 = INTC Tangential power
// 17 = GAUSSC Gaussian power
// 18 = MEANC Monge Mean Curvature, in diopters
// 19 = MONGEA Monge Astigmatism
// 20 = Z Elevation
// 21 = Oblique power or Warp
// third two are colormap to use
// default is USS with fixed range
// 1 = rgb2 discrete heatmap with linear interpolation based on 2 colors
// 2 = rgb5 discrete heatmap with linear interpolation based on 5 colors
// 3 = hsbrgb continuous heatmap with fixed sat and brightness, value mapped to hue
// 4 = gplotpalette discrete heatmap based on 12 colors, no interpolation in the style generated by gnuplot
// 5 = Uniform Standard Scale (USS)palette discrete map with linear interpolation between 26 colors with fixed range for sagittal/axial powers
// 6 = Perceptually Uniform Palette discrete map with linear interpolation between 9 shades with ANSI Z80.3 fixed range
// 7 = USSpalette
// 8 = Perceptually Uniform Palette
// last two digits are the program function
// 0 = open a file, display
// 99 = deallocate arrays for program closure
// 10 = compare two images
// 9 = show zernike coefficents
// 8 = show circumferential ring lsqfillin/splinefillin
// 7 = make lioc
// 6 = make centers
// 5 = make gnuplotsplot
// 4 = redraw without reloading new file
// 3 = write ASCII PLY file
// 2 = write OFF file
// 1 = compute zernike coefficients/maps

int flag=500;
int counter=0;
char *filename;
bool success=false;
bool paintme = false;
int err_janus = 0;

//how very old Fortran that these need to be static & global
// data vectors for corneal images
int nV[3];
int nE[3];
std::vector<GLuint> Elements(26130);
std::vector<GLfloat> Vertices(51840);
GLfloat* vertices = Vertices.data();
GLuint* elements = Elements.data();
std::vector<GLuint> Elements2(26130);
std::vector<GLfloat> Vertices2(51840);
GLfloat* vertices2 = Vertices2.data();
GLuint* elements2 = Elements2.data();
std::vector<GLuint> Elements3(26130);
std::vector<GLfloat> Vertices3(51840);
GLfloat* vertices3 = Vertices3.data();
GLuint* elements3 = Elements3.data();

// data vectors for pupil images
int pupil_nV = 1629;
int pupil_nE = 540;
std::vector<GLuint> pupil_Elements(pupil_nE);
std::vector<GLfloat> pupil_Vertices(pupil_nV);
GLfloat* pupil_vertices = pupil_Vertices.data();
GLuint* pupil_elements = pupil_Elements.data();


// data vector for legend value & colors
int nL = 26*4;
std::vector<float> legendVector(nL);  //26 colors =  1 value + 3 rgbv (value,rgbv)
float* legend = legendVector.data();

// data vector for zernike graph
int nZ = 14;
std::vector<float> zernVector(nZ);  //12 zernike and min/max
float* zern = zernVector.data();

// data vector for legend value & colors
std::vector<float> legendVector2(nL);  //26 colors =  1 value + 3 rgbv (value,rgbv)
float* legend2 = legendVector2.data();

// data vector for zernike graph
std::vector<float> zernVector2(nZ);  //12 zernike and min/max
float* zern2 = zernVector2.data();

QString *m_GLString=nullptr;
QString glstring_global;

class DialogOptionsWidget : public QGroupBox
{
public:
    explicit DialogOptionsWidget(QWidget *parent = nullptr);

    void addCheckBox(const QString &text, int value);
    void addSpacer();
    int value() const;

private:
    typedef QPair<QCheckBox *, int> CheckBoxEntry;
    QVBoxLayout *layout;
    QList<CheckBoxEntry> checkBoxEntries;
};

DialogOptionsWidget::DialogOptionsWidget(QWidget *parent) :
    QGroupBox(parent) , layout(new QVBoxLayout)
{
    setTitle(MainWindow::tr("Options"));
    setLayout(layout);
}

void DialogOptionsWidget::addCheckBox(const QString &text, int value)
{
    QCheckBox *checkBox = new QCheckBox(text);
    layout->addWidget(checkBox);
    checkBoxEntries.append(CheckBoxEntry(checkBox, value));
}

void DialogOptionsWidget::addSpacer()
{
    layout->addItem(new QSpacerItem(0, 0, QSizePolicy::Ignored, QSizePolicy::MinimumExpanding));
}

int DialogOptionsWidget::value() const
{
    int result = 0;
    for (const CheckBoxEntry &checkboxEntry : std::as_const(checkBoxEntries)) {
        if (checkboxEntry.first->isChecked())
            result |= checkboxEntry.second;
    }
    return result;
}


MainWindow::MainWindow(QMainWindow *parent) : assistant(new Assistant)

{
   ui.setupUi(this);
   connect(ui.progressBar, &QProgressBar::valueChanged,this, &MainWindow::updateResult);
   QTimer *timer = new QTimer(this);
   connect(timer, &QTimer::timeout, this, QOverload<>::of(&MainWindow::updateResult));
   timer->start(1000);

   ui.infoLabel->setText(tr("<i>Welcome! Please Open a file.</i>"));
   ui.infoLabel->setFrameStyle(QFrame::StyledPanel | QFrame::Sunken);
   ui.infoLabel->setAlignment(Qt::AlignCenter);

   ui.xSlider->setRange(0, 360 * 16);
   ui.xSlider->setSingleStep(16);
   ui.xSlider->setPageStep(15 * 16);
   ui.xSlider->setTickInterval(15 * 16);
   ui.xSlider->setTickPosition(QSlider::TicksRight);
   ui.ySlider->setRange(0, 360 * 16);
   ui.ySlider->setSingleStep(16);
   ui.ySlider->setPageStep(15 * 16);
   ui.ySlider->setTickInterval(15 * 16);
   ui.ySlider->setTickPosition(QSlider::TicksRight);
   ui.zSlider->setRange(0, 360 * 16);
   ui.zSlider->setSingleStep(16);
   ui.zSlider->setPageStep(15 * 16);
   ui.zSlider->setTickInterval(15 * 16);
   ui.zSlider->setTickPosition(QSlider::TicksRight);
   ui.horizontalSlider->setRange(0,100);
   ui.horizontalSlider->setValue(100);
   connect(ui.horizontalSlider, &QSlider::valueChanged, ui.openGLWidget_2, &GLwidget::settransparency);
   connect(ui.openGLWidget_2, &GLwidget::transparencyChanged, ui.horizontalSlider, &QSlider::setValue);
   connect(ui.xSlider, &QSlider::valueChanged, ui.openGLWidget_2, &GLwidget::setXRotation);
   connect(ui.openGLWidget_2, &GLwidget::xRotationChanged, ui.xSlider, &QSlider::setValue);
   connect(ui.ySlider, &QSlider::valueChanged, ui.openGLWidget_2, &GLwidget::setYRotation);
   connect(ui.openGLWidget_2, &GLwidget::yRotationChanged, ui.ySlider, &QSlider::setValue);
   connect(ui.zSlider, &QSlider::valueChanged, ui.openGLWidget_2, &GLwidget::setZRotation);
   connect(ui.openGLWidget_2, &GLwidget::zRotationChanged, ui.zSlider, &QSlider::setValue);
   ui.xSlider->setValue(0 * 16);
   ui.ySlider->setValue(180 * 16);
   ui.zSlider->setValue(180 * 16);

   createActions();
   createMenus();
   setWindowTitle(tr("Kernunos"));

   setMouseTracking(true);
   qApp->setApplicationDisplayName(tr("kernunos"));

   int frameStyle = QFrame::Sunken | QFrame::Panel;
   errorMessageDialog = new QErrorMessage(this);
   multiLineTextLabel = new QLabel;
   multiLineTextLabel->setFrameStyle(frameStyle);
// starting defaults
   GLwidget::setaxisymmetric(true);
   GLwidget::setlsqvsspline(true);
   centerAct->setEnabled(false);
   ShowZernAct->setEnabled(false);
   ringsAct->setEnabled(false);
   centernodeAct->setEnabled(false);
   adjustradiiAct->setEnabled(false);
   SplinefillinAct->setEnabled(false);
   LSQfillinAct->setEnabled(false);
   lsqvssplineAct->setEnabled(true);

   resize(SCR_WIDTH, SCR_HEIGHT);
   update();
}

//    example usage
//    errorMessageDialog->showMessage(tr("we made an error"));
//   addComments();

void MainWindow::addComments()
{
    bool ok;
    QString text = QInputDialog::getMultiLineText(this, tr("Add Comments"),
                                                  tr("Comments:"), "Add comments here", &ok);
    if (ok && !text.isEmpty())
        multiLineTextLabel->setText(text);
}

void MainWindow::closeEvent(QCloseEvent *)
{
    delete assistant;
}

void MainWindow::SetGLString(QString& gls)
{    m_GLString =  &gls;
   glstring_global=*m_GLString;
}

//https://stackoverflow.com/questions/3418231/replace-part-of-a-string-with-another-string
bool MainWindow::replace(std::string& str,const std::string& from,const std::string& to)
{
    size_t start_pos = str.find(from);
    if(start_pos == std::string::npos) return false;
    str.replace(start_pos,from.length(),to);
    return true;
}

void MainWindow::open()   //multiple invocations needed to make a comparison
{
   ui.infoLabel->setText(tr("Invoked <b>File|Open</b>"));
   flag=flag-(flag%100)+0;  // last two digits of flag=0; need to reset this
   QString filter = "Topography files (*.CUR *.ELE *_CUR.CSV *_ELE.CSV RA*.* XX*.* *OD.CSV *OS.CSV) ;; PentaCam (*.CUR *.ELE *_CUR.CSV *_ELE.CSV);;EyeSys (RA*.* XX*.*);;Atlas (*OD.CSV *OS.CSV);;All (*)";
   QString fileName = QFileDialog::getOpenFileName(this,"Open a file", "", filter);
   if (fileName.isEmpty())
       return;
   QByteArray ba = fileName.toLocal8Bit();
   filename = ba.data();
   ui.infoLabel->setText(tr("filename:  ")+tr(filename));
   std::string str(filename);
   pentacam =  str.find(".CUR")!= std::string::npos || str.find(".ELE") != std::string::npos ||
                   str.find("_CUR")!= std::string::npos || str.find("_ELE") != std::string::npos;
   bool atlas = (str.find("OD.CSV")!= std::string::npos) || (str.find("OS.CSV")!= std::string::npos && !pentacam); //OD.CSV OS.CSV but not _ELE.CSV and _CUR.CSV ->atlas
   bool eyesys= false;               //if substituting XX for RA or RA for XX results in an openable file, then probably EyeSys
   std::string str2(filename);
   if(replace(str2,"RA","XX")) {
       if(FILE *file = fopen(str2.c_str(),"r")) {
           fclose(file); eyesys = true;}
   }else{
       if(replace(str2,"XX","RA")) {
           if(FILE *file = fopen(str2.c_str(),"r")) {
               fclose(file); eyesys = true;
           }}};
   if (!pentacam && !atlas && !eyesys) return;   //no supported file format
   if (pentacam) {
       centerAct->setEnabled(true);
       ShowZernAct->setEnabled(false);
       ringsAct->setEnabled(false);
       centernodeAct->setEnabled(true);
       GLwidget::setCenterNode(true);  //force centernode to center value for pentaca needs modification of SplineCenter to incorporate center value and needs value initialized
       GLwidget::isCenterNode();
       centernodeAct->setChecked(GLwidget::isCenterNode());
       adjustradiiAct->setEnabled(false);
       SplinefillinAct->setEnabled(false);
       LSQfillinAct->setEnabled(false);
       lsqvssplineAct->setEnabled(true);
   };
   if (atlas) {
       ShowZernAct->setEnabled(true);
       centerAct->setEnabled(true);
       ringsAct->setEnabled(true);
       centernodeAct->setEnabled(true);
       adjustradiiAct->setEnabled(true);
       SplinefillinAct->setEnabled(true);
       LSQfillinAct->setEnabled(true);
       lsqvssplineAct->setEnabled(true);
   };
   if (eyesys) {
       ShowZernAct->setEnabled(false);
       centerAct->setEnabled(true);       
       ringsAct->setEnabled(false);
       centernodeAct->setEnabled(true);
       adjustradiiAct->setEnabled(true);
       SplinefillinAct->setEnabled(false);
       LSQfillinAct->setEnabled(false);
       lsqvssplineAct->setEnabled(true);
   };
   compareAct->setEnabled(true);
   decenterAct->setEnabled(true);
   swapAct->setEnabled(true);
   redrawAct->setEnabled(true);
   redrawOptionAct->setEnabled(true);
   gnuplotAct->setEnabled(true);
   liocAct->setEnabled(true);
   makeoffAct->setEnabled(true);
   makeplyAct->setEnabled(true);
   ply2binAct->setEnabled(true);
   off2stlAct->setEnabled(true);
   importexportAct->setEnabled(true);
   zernAct->setEnabled(true);

   if(GLwidget::isconsistency()){
       if (pentacam) GLwidget::setconsistency(false);  //skip the first pentacam file comparison when checking consistency
       if (!fileName.isEmpty()){
           m_GLwidget->DataLoad(fileName, true);}
       if (pentacam) GLwidget::setconsistency(true);
     }
    else{
    if (!fileName.isEmpty())
           m_GLwidget->DataLoad(fileName, true);}
           update();
    if (pentacam) {
       if(GLwidget::isconsistency()){
           std::string str2(filename);
            QString fileName2 = QString::fromStdString(str2);
             if(replace(str2,"_ELE.CSV","_CUR.CSV")) {
               if(FILE *file = fopen(str2.c_str(),"r")) {
                   fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                   fileName2 = QString::fromStdString(str2);
                   if (!fileName2.isEmpty())
                       m_GLwidget->DataLoad(fileName2, true);
               }else{std::cout << "Matching file not found\n" <<std::endl;}
             }else{
             if(replace(str2,"_CUR.CSV","_ELE.CSV")) {
                 if(FILE *file = fopen(str2.c_str(),"r")) {
                   fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                   fileName2 = QString::fromStdString(str2);
                   if (!fileName2.isEmpty())
                       m_GLwidget->DataLoad(fileName2, true);
                 }else{std::cout << "Matching file not found\n" <<std::endl;}
             }}
             if(replace(str2,".ELE",".CUR")) {
                 if(FILE *file = fopen(str2.c_str(),"r")) {
                   fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                   fileName2 = QString::fromStdString(str2);
                   if (!fileName2.isEmpty())
                       m_GLwidget->DataLoad(fileName2, true);
                 }else{std::cout << "Matching file not found\n" <<std::endl;}
             }else{
             if(replace(str2,".CUR",".ELE")) {
                 if(FILE *file = fopen(str2.c_str(),"r")) {
                   fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                   fileName2 = QString::fromStdString(str2);
                   if (!fileName2.isEmpty())
                       m_GLwidget->DataLoad(fileName2, true);
                 }else{std::cout << "Matching file not found\n" <<std::endl;}
             }}
       }
   };
   }

void MainWindow::test()
{
    QString fileName = QString::fromStdString("test");
    ShowZernAct->setEnabled(false);
    centerAct->setEnabled(true);
    ringsAct->setEnabled(false);
    centernodeAct->setEnabled(true);
    adjustradiiAct->setEnabled(true);
    SplinefillinAct->setEnabled(false);
    LSQfillinAct->setEnabled(false);
    lsqvssplineAct->setEnabled(true);
    compareAct->setEnabled(true);
    swapAct->setEnabled(true);
    redrawAct->setEnabled(true);
    redrawOptionAct->setEnabled(true);
    gnuplotAct->setEnabled(true);
    liocAct->setEnabled(true);
    makeoffAct->setEnabled(true);
    makeplyAct->setEnabled(true);
    ply2binAct->setEnabled(true);
    off2stlAct->setEnabled(true);
    importexportAct->setEnabled(true);
    zernAct->setEnabled(true);
    m_GLwidget->DataLoad(fileName, true);
};


void MainWindow::loadFile(QString& fileName, bool filepresent)   //this is for the commandline file if any
{
   if (fileName.isEmpty()) return;
   QByteArray ba = fileName.toLocal8Bit();
   filename = ba.data();
   ui.infoLabel->setText(tr("filename:  ")+tr(filename));
   if (filepresent){
       m_GLwidget->DataLoad(fileName, true);
       std::string str(filename);  
       pentacam =  str.find(".CUR")!= std::string::npos || str.find(".ELE") != std::string::npos ||
                   str.find("_CUR")!= std::string::npos || str.find("_ELE") != std::string::npos;
       bool atlas = (str.find("OD.CSV")!= std::string::npos) || (str.find("OS.CSV")!= std::string::npos && !pentacam); //OD.CSV OS.CSV but not _ELE.CSV and _CUR.CSV ->atlas
       bool eyesys= false;               //if subsitutuing XX for RA or RA for XX results in an openable file, then probably EyeSys
       std::string str2(filename);
       if(replace(str2,"RA","XX")) {
           if(FILE *file = fopen(str2.c_str(),"r")) {
               fclose(file); eyesys = true;}
       }else{
           if(replace(str2,"XX","RA")) {
               if(FILE *file = fopen(str2.c_str(),"r")) {
                   fclose(file); eyesys = true;
               }}};
       if (!pentacam && !atlas && !eyesys) {m_GLwidget->DataLoad(fileName, false); return;}  //cube   //no supported file format
       if (pentacam){
           centerAct->setEnabled(true);  //change to false to not allow for pentacam, center deviations only for Placido
           ShowZernAct->setEnabled(false);
           ringsAct->setEnabled(false);
           centernodeAct->setEnabled(true);
           GLwidget::setCenterNode(true);  //force centernode to center value for pentacam
           GLwidget::isCenterNode();
           centernodeAct->setChecked(GLwidget::isCenterNode());
           adjustradiiAct->setEnabled(false);
           SplinefillinAct->setEnabled(false);
           LSQfillinAct->setEnabled(false);
           lsqvssplineAct->setEnabled(true);
       };       
       if (atlas) {
           ShowZernAct->setEnabled(true);
           centerAct->setEnabled(true);
           ringsAct->setEnabled(true);
           centernodeAct->setEnabled(true);
           adjustradiiAct->setEnabled(true);
           SplinefillinAct->setEnabled(true);
           LSQfillinAct->setEnabled(true);
           lsqvssplineAct->setEnabled(true);
       };
       if (eyesys) {
           ShowZernAct->setEnabled(false);
           centerAct->setEnabled(true);
           ringsAct->setEnabled(false);
           centernodeAct->setEnabled(true);
           adjustradiiAct->setEnabled(true);
           SplinefillinAct->setEnabled(false);
           LSQfillinAct->setEnabled(false);
           lsqvssplineAct->setEnabled(true);
       };
       compareAct->setEnabled(true);
       decenterAct->setEnabled(true);
       swapAct->setEnabled(true);
       redrawAct->setEnabled(true);
       redrawOptionAct->setEnabled(true);
       gnuplotAct->setEnabled(true);
       liocAct->setEnabled(true);
       makeoffAct->setEnabled(true);
       makeplyAct->setEnabled(true);
       ply2binAct->setEnabled(true);
       off2stlAct->setEnabled(true);
       importexportAct->setEnabled(true);
       zernAct->setEnabled(true);


   if(GLwidget::isconsistency()){
       if (pentacam) GLwidget::setconsistency(false);  //skip the first pentacam file comparison when checking consistency
       if (!fileName.isEmpty()){
           m_GLwidget->DataLoad(fileName, true);}
       if (pentacam) GLwidget::setconsistency(true);
   }else{
       if (!fileName.isEmpty())
           m_GLwidget->DataLoad(fileName, true);}
   update();
   if (pentacam) {
       if(GLwidget::isconsistency()){
           std::string str2(filename);
           QString fileName2 = QString::fromStdString(str2);
           if(replace(str2,"_ELE.CSV","_CUR.CSV")) {
               if(FILE *file = fopen(str2.c_str(),"r")) {
                   fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                   fileName2 = QString::fromStdString(str2);
                   if (!fileName2.isEmpty())
                       m_GLwidget->DataLoad(fileName2, true);
               }else{std::cout << "Matching file not found\n" <<std::endl;}
           }else{
               if(replace(str2,"_CUR.CSV","_ELE.CSV")) {
                   if(FILE *file = fopen(str2.c_str(),"r")) {
                       fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                       fileName2 = QString::fromStdString(str2);
                       if (!fileName2.isEmpty())
                           m_GLwidget->DataLoad(fileName2, true);
                   }else{std::cout << "Matching file not found\n" <<std::endl;}
               }}
           if(replace(str2,".ELE",".CUR")) {
               if(FILE *file = fopen(str2.c_str(),"r")) {
                   fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                   fileName2 = QString::fromStdString(str2);
                   if (!fileName2.isEmpty())
                       m_GLwidget->DataLoad(fileName2, true);
               }else{std::cout << "Matching file not found\n" <<std::endl;}
           }else{
               if(replace(str2,".CUR",".ELE")) {
                   if(FILE *file = fopen(str2.c_str(),"r")) {
                       fclose(file); std::cout << "Matching file found" << str2.c_str() << "\n" <<std::endl;
                       fileName2 = QString::fromStdString(str2);
                       if (!fileName2.isEmpty())
                           m_GLwidget->DataLoad(fileName2, true);
                   }else{std::cout << "Matching file not found\n" <<std::endl;}
               }}
       }
   };
}
   else {m_GLwidget->DataLoad(fileName, false);}  //cube
   update();
}

void MainWindow::swap()
{
    flag=flag-(flag%100)+11;  // flag for swap
    QString fileName = "swap";
    QByteArray ba = fileName.toLocal8Bit();
    filename = ba.data();
    janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);
    m_GLwidget->Swap();
}

void MainWindow::compare()
{
    QMessageBox msgBox(QMessageBox::Question, tr("Compare"),
                       tr("Would you like to change the current rotation in degrees"), { }, this);
    msgBox.setInformativeText(tr("The comparison is made with or without pupil alignment/registration " \
                                 "and then rotation counterclockwise around that center in degrees." ));
    msgBox.addButton(QMessageBox::Yes);
    msgBox.addButton(QMessageBox::No);
    msgBox.addButton(QMessageBox::Cancel);
    msgBox.setDefaultButton(QMessageBox::No);
    int pupilvalue = 1;

    QSpacerItem *horizontalspacer = new QSpacerItem(500, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);
    QGridLayout *layout =(QGridLayout*)msgBox.layout();
    layout->addItem(horizontalspacer,layout->rowCount(),0,1,layout->columnCount());
    compareDialogOptionsWidget = new DialogOptionsWidget;
    compareDialogOptionsWidget->addCheckBox(tr("Pupil Registration"), pupilvalue);

    layout->addWidget(compareDialogOptionsWidget);
    int reply = msgBox.exec();
    int degrees;
    degrees=ui.zSlider->value();
    degrees=degrees / 16;

    if (reply == QMessageBox::Yes){
        bool ok;
        degrees = QInputDialog::getInt(this, tr("Rotation "),
                                                 tr("Degrees:"), degrees, 0, 360, 1, &ok,
                                                 Qt::WindowFlags());
    }
    else if (reply == QMessageBox::No){
    }
    if (!(reply == QMessageBox::Cancel)){

    ui.zSlider->setValue(degrees * 16);
    flag=flag-(flag%100)+10;  // last two digits of flag=10 is the code for compare

//  for now set compare to hsbrgb
    int map=(flag-(flag%100))/100%100 ; //save the current selection
    GLwidget::setAllmapsfalse();
    GLwidget::sethsbrgb(true);
    checkmapsflags();

//  pupil register is sent through changing dat
    if (compareDialogOptionsWidget->value()){GLwidget::setpupilregister(true);}
    else {GLwidget::setpupilregister(false);}

    QString fileName=QString("%1").arg(degrees);  //pass the degrees with the filename
    m_GLwidget->DataLoad(fileName,true);

//  for now restore FROM hsbrgb
    flag=flag+100*(map-3);
    GLwidget::sethsbrgb(false);
    if (map == 1) GLwidget::setrgb2(true);
    if (map == 2) GLwidget::setrgb5(true);
    if (map == 3) GLwidget::sethsbrgb(true);
    if (map == 4) GLwidget::setgplotpalette(true);
    if (map == 5) GLwidget::setUSSfixed(true);
    if (map == 6) GLwidget::setperceptualuniformfixed(true);
    if (map == 7) GLwidget::setUSSpalette(true);
    if (map == 8) GLwidget::setperceptualuniformpalette(true);
    checkmapsflags();
    update();}
    else {
//        ui.infoLabel->setText(tr("Cancel"));
        return;
    }
}

void MainWindow::decenter()
{
    QMessageBox msgBox(QMessageBox::Question, tr("Decenter"),
                       tr("Would you like to change the current center"), { }, this);
    msgBox.setInformativeText(tr("Allows for decentering the data " ));
    msgBox.addButton(QMessageBox::Yes);
    msgBox.addButton(QMessageBox::No);
    msgBox.addButton(QMessageBox::Cancel);
    msgBox.setDefaultButton(QMessageBox::No);
    int polar = 1;

    QSpacerItem *horizontalspacer = new QSpacerItem(500, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);
    QGridLayout *layout =(QGridLayout*)msgBox.layout();
    layout->addItem(horizontalspacer,layout->rowCount(),0,1,layout->columnCount());
    decenterDialogOptionsWidget = new DialogOptionsWidget;
    decenterDialogOptionsWidget->addCheckBox(tr("Polar"), polar);
    layout->addWidget(decenterDialogOptionsWidget);
    int reply = msgBox.exec();
    int degrees = 0;
    double dist = 0.0;
    double xdist = 0.0;
    double ydist = 0.0;
    //  polar vs cartesian is sent through changing dat
        if (reply == QMessageBox::Yes){
            GLwidget::setdecenter(true);  //set decenter flag (chnages dat)
            bool ok;
            if (decenterDialogOptionsWidget->value()){

             dist = QInputDialog::getDouble(this, tr("Distance "),
                                           tr("mm:"), dist, 0.0, 0.5, 2, &ok,
                                           Qt::WindowFlags());
             degrees = QInputDialog::getInt(this, tr("Rotation "),
                                           tr("Degrees:"), degrees, 0, 360, 1, &ok,
                                           Qt::WindowFlags());
            if (ok){
                xdist = dist * cos(3.1415926*degrees/180.0);
                ydist = dist * sin(3.1415926*degrees/180.0);}
             }
             else {
            xdist = QInputDialog::getDouble(this, tr("x dist "),
                         tr("mm:"), xdist, -0.5, 0.5, 2, &ok, Qt::WindowFlags());
            ydist = QInputDialog::getDouble(this, tr("y dist "),
                         tr("mm:"), ydist, -0.5, 0.5, 2, &ok, Qt::WindowFlags());}
            }

        else {
            if (reply == QMessageBox::No) {GLwidget::setdecenter(false);
            }};

    if (!(reply == QMessageBox::Cancel)){

        QString fileName = QString::fromStdString((std::to_string(xdist)+","+std::to_string(ydist))); //send floats as filename
        flag=flag-(flag%100)+4;  // last two digits of flag=4; this is a type of redraw;
        QByteArray ba = fileName.toLocal8Bit();
        filename = ba.data();

        janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);

        update();
        GLwidget::setdecenter(false);}   //reset decenter flag
    else {
        //        ui.infoLabel->setText(tr("Cancel"));
        return;
     }
}

void MainWindow::redraw(){
   flag=flag-(flag%100)+4;  // last two digits of flag=4;
    QString fileName = "redraw";
    QByteArray ba = fileName.toLocal8Bit();
    filename = ba.data();

   janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);

   return;}

void MainWindow::zerncompute()
{
    if (system(NULL)) puts (" gnuplot available");
    else exit (EXIT_FAILURE);
    if(system("command -v gnuplot > /dev/null 2>&1") ){
        std::cout << "'gnuplot' command is not available.\n";
        ui.infoLabel->setText(tr("gnuplot call failed!"));
        return;
    }
    QTemporaryFile FILE;
    FILE.setAutoRemove(true);
    FILE.open();
    QString filenamelocal = FILE.fileName();
    filenamelocal = filenamelocal.append(".gnu");
    QByteArray ba = filenamelocal.toLocal8Bit();
    filename = ba.data();
    flag=flag-(flag%100)+1;  // last two digits of flag=1;
    std::thread([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);}).detach();

#ifdef _WIN32
    // For Windows, prompt for a keystroke before the Gnuplot object goes out of scope so that
    // the gnuplot window doesn't get closed.
    std::cout << "Press enter to exit." << std::endl;
    std::cin.get();
#endif

    Z44VerticalQuatrafoilAct->setEnabled(true);
    Z42Vertical2ndAstigAct->setEnabled(true);
    Z40SphericalAberrationAct->setEnabled(true);
    Z4neg2Oblique2ndAstigAct->setEnabled(true);
    Z4neg4ObliqueQuatrafoilAct->setEnabled(true);
    Z33ObliqueTrefoilAct->setEnabled(true);
    Z3neg3VerticalTrefoilAct->setEnabled(true);
    Z31HorizontalComaAct->setEnabled(true);
    Z3neg1VerticalComaAct->setEnabled(true);
    Z22VerticalAstigAct->setEnabled(true);
    Z2neg2ObliqueAstigAct->setEnabled(true);
    Z20DefocusAct->setEnabled(true);
    Z11XtiltAct->setEnabled(true);
    Z1neg1YtiltAct->setEnabled(true);
    Z00PistonAct->setEnabled(true);
    ShowZernAct->setEnabled(true);
    ui.infoLabel->setText(tr("Invoked <b>zernike</b>"));
    return;
}

void MainWindow::showzern()
{
    if (system(NULL)) puts (" gnuplot available");
    else exit (EXIT_FAILURE);
    if(system("command -v gnuplot > /dev/null 2>&1") ){
        std::cout << "'gnuplot' command is not available.\n";
        ui.infoLabel->setText(tr("gnuplot call failed!"));
        return;
    }
    QTemporaryFile FILE;
    FILE.setAutoRemove(true);
    FILE.open();
    QString filenamelocal = FILE.fileName();
    filenamelocal = filenamelocal.append(".gnu");
    QByteArray ba = filenamelocal.toLocal8Bit();
    filename = ba.data();
    flag=flag-(flag%100)+9;  // last two digits of flag=1;
    auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
    future1.get();

#ifdef _WIN32
    // For Windows, prompt for a keystroke before the Gnuplot object goes out of scope so that
    // the gnuplot window doesn't get closed.
    std::cout << "Press enter to exit." << std::endl;
    std::cin.get();
#endif
    ui.infoLabel->setText(tr("Invoked <b>Show Zernike</b>"));
    return;
}

void MainWindow::importexport()
{
    //make temporary PLY file name
    QTemporaryFile FILE;
    FILE.setAutoRemove(true);
    FILE.open();
    QString filenamelocal = FILE.fileName();
    filenamelocal = filenamelocal.append(".ply");
    QByteArray ba = filenamelocal.toLocal8Bit();
    filename = ba.data();
    // generate temp ply file
    flag=flag-(flag%100)+3;  // last two digits of flag=3;
    auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
    future1.get();
    // get output file name and type
   QString filter =
       "Stanford Polygon Library ASCII .ply (*.ply) ;; "
       "Stereolithography .stl (*.stl) ;; "
       "Stereolithography binary without color .stlb (*.stlb) ;; "
       "Extensible 3D .x3d (*.x3d) ;; "
       "Direct3D XFile .x (*.x) ;; "
       "Autodesk FBX .fbx (*.fbx) ;; "
       "Wavefront Object .obj (*.obj) ;; "
       "Collada .dae (*.dae) ;; "
       "3D Manufacturing Consortium .3mf (*.3mf) ;; "
       "GL binary .glb (*.glb) ;; "
       "GL Transmission Format .gltf (*.gltf) ;; "
       "Discreet 3DS .3ds (*.3ds) )";
   QString fileName = QFileDialog::getSaveFileName(this,"Write Assimp exports", "", filter);
   if (fileName.isEmpty())
       return;
   ba = fileName.toLocal8Bit();
   const char *filenameout = ba.data();
   const char* extension = strrchr(filenameout, '.');
//Assimp code here
   auto stream = aiGetPredefinedLogStream(aiDefaultLogStream_STDOUT,NULL);
   // auto stream = aiGetPredefinedLogStream(aiDefaultLogStream_FILE,"assimp_log.txt");
   aiAttachLogStream(&stream);
   Assimp::Importer Importer;
   Assimp::Exporter Exporter;
   const aiImporterDesc *iformat = nullptr;
   const aiExportFormatDesc *format;
   // Check and validate the specified model file extension.
   // only obj,dae,ascii ply,binary and ascii stl,3ds,x and fbx verified to be importable
   // in meshlab using ply as import file
   // Colors are not always preserved eg. stl
   // this is a special case that precedes the valid extension test
   std::string extstring = extension;
   std::string binarystl = ".stlb";
   if (extstring == binarystl) {
       //because stlb is not recognized, and neither is bin.stl as binary stl
       std::cout << "\tReading file using ASSIMP" << std::endl;
       const aiScene *aiscene = Importer.ReadFile(filename, aiProcess_ValidateDataStructure | aiProcessPreset_TargetRealtime_MaxQuality);
       if (!aiscene) {
            printf("Error parsing '%s': '%s'\n", filename, Importer.GetErrorString()); }
       format = Exporter.GetExportFormatDescription(6);
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
       return;
   }
   // this is a special case that precedes the valid extension test
   extstring = extension;
   std::string binarygl = ".glb";
   if (extstring == binarygl) {
       //because glb is ambiguously recognized together with gltf
       std::cout << "\tReading file using ASSIMP" << std::endl;
       const aiScene *aiscene = Importer.ReadFile(filename, aiProcess_ValidateDataStructure | aiProcessPreset_TargetRealtime_MaxQuality);
       if (!aiscene) {
           printf("Error parsing '%s': '%s'\n", filename, Importer.GetErrorString()); }
       format = Exporter.GetExportFormatDescription(11); // 13 or 11(glb2) 13 not accepted by meshlab
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
       return;
   }
   // this is a special case that precedes the valid extension test
   extstring = extension;
   std::string asciigl = ".gltf";
   if (extstring == asciigl) {
       //because glb is ambiguously recognized together with gltf
       std::cout << "\tReading file using ASSIMP" << std::endl;
       const aiScene *aiscene = Importer.ReadFile(filename, aiProcess_ValidateDataStructure | aiProcessPreset_TargetRealtime_MaxQuality);
       if (!aiscene) {
           printf("Error parsing '%s': '%s'\n", filename, Importer.GetErrorString()); }
       format = Exporter.GetExportFormatDescription(10);  // 12 or 10(gltf2) 12 not accepted by meshlab
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
       return;
   }
   // this is a special case that precedes the valid extension test
   // https://github.com/assimp/assimp/issues/3827  binary ply is broken, use rply instead
   /*
   extstring = extension;
   std::string binaryply = ".plyb";
   if (extstring == binaryply) {
       //because plyb is not recognized, and neither is bin.ply as binary ply
       std::cout << "\tReading file using ASSIMP" << std::endl;
       const aiScene *aiscene = Importer.ReadFile(filename, aiProcess_ValidateDataStructure | aiProcessPreset_TargetRealtime_MaxQuality);
       if (!aiscene) {
            printf("Error parsing '%s': '%s'\n", filename, Importer.GetErrorString()); }
       format = Exporter.GetExportFormatDescription(8);
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
       return;
   }
*/
   if (!extension) {
       std::cout <<"Please provide a file with a valid extension.\n";
       return;
   }
   if (AI_FALSE == aiIsExtensionSupported(extension)) {
       std::cout <<"The specified model file extension is currently unsupported in Assimp\n ";
       return;
   }
   std::cout << "\tReading file using ASSIMP" << std::endl;
   const aiScene *aiscene = Importer.ReadFile(filename, aiProcess_ValidateDataStructure | aiProcessPreset_TargetRealtime_MaxQuality);
   if (!aiscene) {
       printf("Error parsing '%s': '%s'\n", filename, Importer.GetErrorString());
       return;}
   uint ID = Importer.GetImporterIndex(extension);  //unfortunately these are not Exporter formatIDs
   uint i = 0 ;
   /*
   uint countin = Importer.GetImporterCount();
   do {
       iformat = Importer.GetImporterInfo(i);
       std::cout << "Import id: "<< i << " " << iformat->mFileExtensions << "\n";
       i++;
   } while (i < countin);
   */
   std::cout << "Import ID: "<< ID << "\n";
   iformat = Importer.GetImporterInfo(ID);
   uint count = Exporter.GetExportFormatCount();
//   std::cout << "import ID count: "<< count << "\n";
   i = 0 ;
   do {
       format = Exporter.GetExportFormatDescription(i);
 //      std::cout << "Export id: "<< i << " " << format->id << "\n";
       if (iformat->mFileExtensions == format->id){
            std::cout << "Export ID is " << i << "\n";
            Exporter.Export(aiscene, format->id , filenameout, 0);
            std::cout << "Wrote " << filenameout << "\n";
       }
       i++;
   } while (i < count);
   // special cases that have mismatched import and export descriptions
   // the import ID is used in order to identify the format from the extension
   // even though I am always actually importing PLY,
   // otherwise have to manually check the extension as with stlb/gltf/glb above prior to the extension test
   if (ID == 26) {  // dae or collada import ID = 26
       format = Exporter.GetExportFormatDescription(0);  // export ID = 0
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
   }
   if (ID == 45) {  //x3d import ID =45
       format = Exporter.GetExportFormatDescription(16);  //export ID = 16
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
   }
   if (ID == 44) { //3mf import ID = 44
       format = Exporter.GetExportFormatDescription(19);  //export OD = 19
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
   }
   if (ID == 3) { //3ds import ID =3
       format = Exporter.GetExportFormatDescription(9); //export ID = 9
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
   }
   if (!(aiReturn_SUCCESS == 0)) {
       std::cout << "Error exporting" << filenameout << Exporter.GetErrorString() << "\n" ;
   }
   aiDetachAllLogStreams();
   update();
}

void MainWindow::ply2bin()
{
    QString filter = "binary PLY *.bin.ply (*.bin.ply)";
    QString fileName = QFileDialog::getSaveFileName(this,"Write to binary PLY .bin.ply", "", filter);
    if (fileName.isEmpty())
      return;
    QByteArray ba = fileName.toLocal8Bit();
    char *filenameout = ba.data();
    //make temporary PLY file
    /*  //without Qt
    std::string filenamelocal = std::tmpnam(nullptr);
    filenamelocal = filenamelocal.append(".ply");
    std::cout << "temporary file name: " << filenamelocal << '\n';
    const char *filename = filenamelocal.c_str();
    */
    QTemporaryFile FILE;
    FILE.setAutoRemove(true);
    FILE.open();
    QString filenamelocal = FILE.fileName();
    filenamelocal = filenamelocal.append(".ply");
    ba = filenamelocal.toLocal8Bit();
    filename = ba.data();
    flag=flag-(flag%100)+3;  // last two digits of flag=3;
    auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
    future1.get();
    // from https://w3.impa.br/~diego/software/rply/ c program to convert ASCII PLY to binary PLY; MIT licence, included source in tree
    int wrote=ConvertPLYtoBIN(filename,filenameout);
    if (wrote == 0) {
      ui.infoLabel->setText(tr("Wrote  ")+tr(filenameout)); }
    else {
      ui.infoLabel->setText(tr("Failed to write  ")+tr(filenameout));}
   update();
}

void MainWindow::off2stl()
{
   QString filter = "STL *.stl  (*.stl) ;; Binary with color *.bin.stl (*.bin.stl)";
   QString fileName = QFileDialog::getSaveFileName(this,"Write to an ASCII STL or Binary (with color) STL file", "", filter);
   if (fileName.isEmpty())
      return;
   QByteArray ba = fileName.toLocal8Bit();
   char *filenameout1 = ba.data();
      ui.infoLabel->setText(tr("selected  ")+tr(filenameout1));
   //make temporary OFF file
   /*  //without Qt
    std::string filenamelocal = std::tmpnam(nullptr);
    filenamelocal = filenamelocal.append(".off");
    std::cout << "temporary file name: " << filenamelocal << '\n';
    const char *filename = filenamelocal.c_str();
    */
   QTemporaryFile FILE;
   FILE.setAutoRemove(true);  //doesnt do anything
   FILE.open();
   QString filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".off");
   ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   flag=flag-(flag%100)+2;  // last two digits of flag=2;
   auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
   future1.get();

   std::string str(filenameout1);
   bool binary = str.find(".bin.stl")!= std::string::npos;;
//   std::cout << str << " " << binary << std::endl;   //filenameout1 still here

   int deftype =0;
   if(binary){
       QMessageBox msgBox(QMessageBox::Question, tr("Binary STL color"),
                          tr("Do you want VisCam/SolidView compatible color (Yes),Materials Magic compatible color (No) or no color (Cancel)"), { }, this);
       msgBox.setInformativeText(tr("This choice does not affect ASCII STL, which never have color information" ));
       msgBox.addButton(QMessageBox::Yes);
       msgBox.addButton(QMessageBox::No);
       msgBox.addButton(QMessageBox::Cancel);
       msgBox.setDefaultButton(QMessageBox::Yes);
       int reply = msgBox.exec();
       if (reply == QMessageBox::Yes) {deftype=0;}
       if (reply == QMessageBox::No) {deftype=1;}
       if (reply == QMessageBox::Cancel) {deftype=2;}
   }
// had to do this because filenameout1 disappears when the above if(binary) stanza exists after defining ??!!
   ba = fileName.toLocal8Bit();
   char *filenameout = ba.data();

//   std::string str2(filenameout);
//   std::cout << str2 << " " << deftype << std::endl;
//   std::string str3(filenameout1);
//   std::cout << str3 << " " << deftype << std::endl; //filenameout1 gone, unless I define filenameout!

   ConvertOFFtoSTL_C_(filename,filenameout,&deftype);
   ui.infoLabel->setText(tr("Wrote  ")+tr(filenameout));
   FILE.remove();    //doesnt do anything
}

void MainWindow::makeoff()
{
   QString filter = "OFF *.off (*.off) ";
   QString fileName = QFileDialog::getSaveFileName(this,"Write to an OFF file", "", filter);
   if (fileName.isEmpty())
      return;
   QByteArray ba = fileName.toLocal8Bit();
   filename = ba.data();
   flag=flag-(flag%100)+2;  // last two digits of flag=2;
   auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
   future1.get();
   ui.infoLabel->setText(tr("Wrote  ")+tr(filename));
}

void MainWindow::makeply()
{
   QString filter = "PLY *.ply  (*.ply) ";
   QString fileName = QFileDialog::getSaveFileName(this,"Write to an ASCII PLY file", "", filter);
   if (fileName.isEmpty())
      return;
   QByteArray ba = fileName.toLocal8Bit();
   filename = ba.data();
   flag=flag-(flag%100)+3;  // last two digits of flag=3;
   auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
   future1.get();
   ui.infoLabel->setText(tr("Wrote  ")+tr(filename));
}

void MainWindow::LinesofCurvature()
{
    QTemporaryFile FILE;
    FILE.setAutoRemove(true);  //does not do anything
    FILE.open();
    QString filenamelocal = FILE.fileName();
    filenamelocal = filenamelocal.append(".car");
    QByteArray ba = filenamelocal.toLocal8Bit();
    filename = ba.data();
    flag=flag-(flag%100)+7;  // last two digits of flag=7;
    auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
    future1.get();
    int wrote=lioc(filename);
   if (wrote == 0) {
       ui.infoLabel->setText(tr("gnuplot called successfully for lioc  ")); }
   else {
       ui.infoLabel->setText(tr("gnuplot call failed!"));}
}

void MainWindow::gnuplotsplot() {

   if (system(NULL)) puts (" gnuplot available");
   else exit (EXIT_FAILURE);
   if(system("command -v gnuplot > /dev/null 2>&1") ){
       std::cout << "'gnuplot' command is not available.\n";
       ui.infoLabel->setText(tr("gnuplot call failed!"));
       return;
   }
   QTemporaryFile FILE;
   FILE.setAutoRemove(true);  //does not do anything
   FILE.open();
   QString filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".gnu");
   QByteArray ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   flag=flag-(flag%100)+5;  // last two digits of flag=5;
   auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
   future1.get();
   // would be better if calcs could be done here instead of in janus
   Gnuplot gp;
   gp << "load \"" << filename << "\n";

#ifdef _WIN32
   // For Windows, prompt for a keystroke before the Gnuplot object goes out of scope so that
   // the gnuplot window doesn't get closed.
   std::cout << "Press enter to exit." << std::endl;
   std::cin.get();
#endif
   ui.infoLabel->setText(tr("gnuplot called successfully  "));
   return;
}

void MainWindow::center() {

   if (system(NULL)) puts (" gnuplot available");
   else exit (EXIT_FAILURE);
   if(system("command -v gnuplot > /dev/null 2>&1") ){
       std::cout << "'gnuplot' command is not available.\n";
       ui.infoLabel->setText(tr("gnuplot call failed!"));
       return;
   }
   QTemporaryFile FILE;
   FILE.setAutoRemove(true);  //does not do anything
   FILE.open();
   QString filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".gnu");
   QByteArray ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   flag=flag-(flag%100)+6;  // last two digits of flag=6;
   auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
   future1.get();
   // would be better if calcs could be done here instead of in janus, or at least call WriteCenter?

   Gnuplot gp;
   gp << "reset\n";
   gp << "set polar\n";

   gp << "set term wxt 1 title 'MeanC' \n";
   filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".mea");
   ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   gp << "plot \"" << filename << "\" using 1:2 with lines title" <<'"'<< "MeanC" << '"' << "\n";

   gp << "set term wxt 2 title 'MongeA' \n";
   filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".mon");
   ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   gp << "plot \"" << filename << "\" using 1:2 with lines title" <<'"'<< "MongeA" << '"' << "\n";

   gp << "set term wxt 3 title 'IntC' \n";
   filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".int");
   ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   gp << "plot \"" << filename << "\" using 1:2 with lines title" <<'"'<< "IntC" << '"' << "\n";

   gp << "set term wxt 4 title 'SagC' \n";
   filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".sag");
   ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   gp << "plot \"" << filename << "\" using 1:2 with lines title" <<'"'<< "SagC" << '"' << "\n";

   if (!pentacam){
   gp << "set term wxt 5 title 'Center' \n";
   filenamelocal = FILE.fileName();
   filenamelocal = filenamelocal.append(".plt");
   ba = filenamelocal.toLocal8Bit();
   filename = ba.data();
   gp << "plot \"" << filename << "\" using 1:2 with lines title" <<'"'<< "Center" << '"' << "\n";
   }

#ifdef _WIN32
   // For Windows, prompt for a keystroke before the Gnuplot object goes out of scope so that
   // the gnuplot window doesn't get closed.
   std::cout << "Press enter to exit." << std::endl;
   std::cin.get();
#endif
   ui.infoLabel->setText(tr("gnuplot called successfully  "));
   return;
}

void MainWindow::rings() {

    if (system(NULL)) puts (" gnuplot available");
    else exit (EXIT_FAILURE);
    if(system("command -v gnuplot > /dev/null 2>&1") ){
        std::cout << "'gnuplot' command is not available.\n";
        ui.infoLabel->setText(tr("gnuplot call failed!"));
        return;
    }
    QTemporaryFile FILE;
    FILE.setAutoRemove(true);  //does not do anything
    FILE.open();
    QString filenamelocal = FILE.fileName();
    filenamelocal = filenamelocal.append(".gnu");
    QByteArray ba = filenamelocal.toLocal8Bit();
    filename = ba.data();
    flag=flag-(flag%100)+8;  // last two digits of flag=8;
    auto future1 = std::async([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE,&err_janus);});
    future1.get();
    // would be better if calcs could be done here instead of in janus, or at least call WriteCenter?

    Gnuplot gp;
    gp << "reset\n";
    gp << "set term wxt 1 title 'Rings' \n";
    filenamelocal = FILE.fileName();
    filenamelocal = filenamelocal.append(".plt");
    ba = filenamelocal.toLocal8Bit();
    filename = ba.data();
    gp << "set polar \n";
    gp << "plot \"" << filename << "\" using 1:2 title" <<'"'<< "Rings" << '"' << "\n";
#ifdef _WIN32
    // For Windows, prompt for a keystroke before the Gnuplot object goes out of scope so that
    // the gnuplot window doesn't get closed.
    std::cout << "Press enter to exit." << std::endl;
    std::cin.get();
#endif
    ui.infoLabel->setText(tr("gnuplot called successfully  "));
    return;
}


void MainWindow::checkfctsflags(){
    AxialAct->setChecked(GLwidget::isAxial());
    ObliqueAct->setChecked(GLwidget::isOblique());
    TangentialAct->setChecked(GLwidget::isTangential());
    GaussianAct->setChecked(GLwidget::isGaussian());
    MeanAct->setChecked(GLwidget::isMean());
    AstigAct->setChecked(GLwidget::isAstig());
    ElevationAct->setChecked(GLwidget::isElevation());
    Z44VerticalQuatrafoilAct->setChecked(GLwidget::isZ44());
    Z42Vertical2ndAstigAct->setChecked(GLwidget::isZ42());
    Z40SphericalAberrationAct->setChecked(GLwidget::isZ40());
    Z4neg2Oblique2ndAstigAct->setChecked(GLwidget::isZ4neg2());
    Z4neg4ObliqueQuatrafoilAct->setChecked(GLwidget::isZ4neg4());
    Z33ObliqueTrefoilAct->setChecked(GLwidget::isZ33());
    Z31HorizontalComaAct->setChecked(GLwidget::isZ31());
    Z3neg1VerticalComaAct->setChecked(GLwidget::isZ3neg1());
    Z3neg3VerticalTrefoilAct->setChecked(GLwidget::isZ3neg3());
    Z22VerticalAstigAct->setChecked(GLwidget::isZ22());
    Z20DefocusAct->setChecked(GLwidget::isZ20());
    Z2neg2ObliqueAstigAct->setChecked(GLwidget::isZ2neg2());
    Z11XtiltAct->setChecked(GLwidget::isZ11());
    Z1neg1YtiltAct->setChecked(GLwidget::isZ1neg1());
    Z00PistonAct->setChecked(GLwidget::isPiston());
    return;
}

void MainWindow::checkmapsflags(){
    rgb2Act->setChecked(GLwidget::isrgb2());
    rgb5Act->setChecked(GLwidget::isrgb5());
    hsbrgbAct->setChecked(GLwidget::ishsbrgb());
    gplotpaletteAct->setChecked(GLwidget::isgplotpalette());
    USSPaletteAct->setChecked(GLwidget::isUSSpalette());
    USSfixedAct->setChecked(GLwidget::isUSSfixed());
    PerceptuallyUniformPaletteAct->setChecked(GLwidget::isperceptualuniformpalette());
    PerceptualfixedAct->setChecked(GLwidget::isperceptualuniformfixed());
}

void MainWindow::consistency()
{
    if (GLwidget::isconsistency()) {
        GLwidget::setconsistency(false);
        ui.infoLabel->setText(tr("Set <b>View:check consistency</b>"));
    } else {
        GLwidget::setconsistency(true);
        ui.infoLabel->setText(tr("Set <b>View:check consistency</b>"));
    };
}

void MainWindow::light()
{
   if (GLwidget::isLight()) {
        GLwidget::setLight(false);
        ui.infoLabel->setText(tr("Set <b>View:Lighting false</b>"));
   } else {
        GLwidget::setLight(true);
        ui.infoLabel->setText(tr("Set <b>View:Lighting true</b>"));
   };
}

// Auto Redraw on all changes to tweaks, functions or colors
void MainWindow::redrawOption()
{
    if (GLwidget::isRedraw()) {
        GLwidget::setRedraw(false);
        ui.infoLabel->setText(tr("Set <b>View:Auto Redraw false</b>"));
    } else {
        GLwidget::setRedraw(true);
        ui.infoLabel->setText(tr("Set <b>View:Auto Redraw true</b>"));
    };
}

void MainWindow::normal()
{
   if (GLwidget::isNormal()) {
        GLwidget::setNormal(false);
        ui.infoLabel->setText(tr("Set <b>View:Normal false</b>"));
   } else {
        GLwidget::setNormal(true);
        ui.infoLabel->setText(tr("Set <b>View:Normal true</b>"));
   };
}

void MainWindow::pupil()
{
    if (GLwidget::isPupil()) {
        GLwidget::setPupil(false);
        ui.infoLabel->setText(tr("Set <b>View:Pupil false</b>"));
    } else {
        GLwidget::setPupil(true);
        ui.infoLabel->setText(tr("Set <b>View:Pupil true</b>"));
    };
}


void MainWindow::fctAxial()
{
   if (GLwidget::isAxial()) {
        GLwidget::setAxial(true);  //cannot turn off without turning something else on
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Axial is default, set another to deselect</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setAxial(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Axial true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctOblique()
{
    if (GLwidget::isOblique()) {
        GLwidget::setOblique(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Oblique false, reset to Axial</b>"));
    } else {
        GLwidget::setAllfctfalse();
        GLwidget::setOblique(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Oblique true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
    };
}


void MainWindow::fctTangential()
{
   if (GLwidget::isTangential()) {
        GLwidget::setTangential(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Tangential false, reset to Axial</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setTangential(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Tangential true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctGaussian()
{
   if (GLwidget::isGaussian()) {
        GLwidget::setGaussian(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Gaussian false, reset to Axial</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setGaussian(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Gaussian true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctMean()
{
   if (GLwidget::isMean()) {
        GLwidget::setMean(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Mean false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setMean(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Mean true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctMongeAstig()
{
   if (GLwidget::isAstig()) {
        GLwidget::setAstig(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Monge Astigmatism false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setAstig(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Monge Astigmatism true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctElevation()
{
   if (GLwidget::isElevation()) {
        GLwidget::setElevation(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Elevation false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setElevation(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Elevation true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ44()
{
   if (GLwidget::isZ44()) {
        GLwidget::setZ44(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Vertical Quatrafoil false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ44(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Vertical Quatrafoil true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ42()
{
   if (GLwidget::isZ42()) {
        GLwidget::setZ42(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Vertical 2nd Astig false</b>"));
   } else {
        GLwidget::setAllfctfalse();;
        GLwidget::setZ42(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Vertical 2nd Astig true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ40()
{
   if (GLwidget::isZ40()) {
        GLwidget::setZ40(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Spherical Aberration false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ40(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Spherical Aberration true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ4neg4()
{
   if (GLwidget::isZ4neg4()) {
        GLwidget::setZ4neg4(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Oblique Quatrafoil false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ4neg4(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Oblique Quatrafoil true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ4neg2()
{
   if (GLwidget::isZ4neg2()) {
        GLwidget::setZ4neg2(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Oblique 2nd Astig false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ4neg2(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Oblique 2nd Astig true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ33()
{
   if (GLwidget::isZ33()) {
        GLwidget::setZ33(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Oblique Trefoil false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ33(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Oblique Trefoil true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ31()
{
   if (GLwidget::isZ31()) {
        GLwidget::setZ31(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Horizontal Coma false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ31(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Horizontal Coma true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ3neg1()
{
   if (GLwidget::isZ3neg1()) {
        GLwidget::setZ3neg1(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Vertical Coma false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ3neg1(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Vertical Coma true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ3neg3()
{
   if (GLwidget::isZ3neg3()) {
        GLwidget::setZ3neg3(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Vertical Trefoil false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ3neg3(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Vertical Trefoil true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ22()
{
   if (GLwidget::isZ22()) {
        GLwidget::setZ22(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Vertical Astig false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ22(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Vertical Astig true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ20()
{
   if (GLwidget::isZ20()) {
        GLwidget::setZ20(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Defocus false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ20(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Defocus true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ2neg2()
{
   if (GLwidget::isZ2neg2()) {
        GLwidget::setZ2neg2(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Oblique Astig false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ2neg2(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Oblique Astig true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ11()
{
   if (GLwidget::isZ11()) {
        GLwidget::setZ11(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:X-tilt false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ11(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:X-tilt true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ1neg1()
{
   if (GLwidget::isZ1neg1()) {
        GLwidget::setZ1neg1(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Y-tilt false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setZ1neg1(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Y-tilt true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::fctZ00()
{
   if (GLwidget::isPiston()) {
        GLwidget::setPiston(false);
        GLwidget::setAxial(true);
        AxialAct->setChecked(GLwidget::isAxial());
        ui.infoLabel->setText(tr("Set <b>View:Piston false</b>"));
   } else {
        GLwidget::setAllfctfalse();
        GLwidget::setPiston(true);
        checkfctsflags();
        ui.infoLabel->setText(tr("Set <b>View:Piston true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::tweakcenterNode()
{
   if (GLwidget::isCenterNode()) {
        GLwidget::setCenterNode(false);
        centerAct->setChecked(GLwidget::isCenterNode());
        ui.infoLabel->setText(tr("Set <b>Tweak:Center Node false</b>"));
   } else {
        GLwidget::setCenterNode(true);
        centerAct->setChecked(GLwidget::isCenterNode());
        ui.infoLabel->setText(tr("Set <b>Tweak:Center Node true</b>"));
   };
   if (GLwidget::isRedraw()) {
       redraw();
   };
}

void MainWindow::tweakadjustradii()
{
   if (GLwidget::isadjustradii()) {
        GLwidget::setadjustradii(false);
        adjustradiiAct->setChecked(GLwidget::isadjustradii());
        ui.infoLabel->setText(tr("Set <b>Tweak:Adjust radii false</b>"));
   } else {
        GLwidget::setadjustradii(true);
        adjustradiiAct->setChecked(GLwidget::isadjustradii());
        ui.infoLabel->setText(tr("Set <b>Tweak:Adjust radii true</b>"));
   };
   if (GLwidget::isRedraw()) {
       redraw();
   };
}

void MainWindow::tweakcubic()
{
   if (GLwidget::iscubic()) {
        GLwidget::setcubic(false);
        cubicAct->setChecked(GLwidget::iscubic());
        ui.infoLabel->setText(tr("Set <b>Tweak:Default Trapezoidal Integration</b>"));
   } else {
        GLwidget::setcubic(true);
        cubicAct->setChecked(GLwidget::iscubic());
        ui.infoLabel->setText(tr("Set <b>Tweak:Cubic Spline Integration</b>"));
   };
   if (GLwidget::isRedraw()) {
       redraw();
   };
}

void MainWindow::tweakLSQfill()
{
   if (GLwidget::isLSQfillin()) {
        GLwidget::setLSQfillin(false);
        LSQfillinAct->setChecked(GLwidget::isLSQfillin());
        SplinefillinAct->setChecked(GLwidget::isSplinefillin());
        ui.infoLabel->setText(tr("Set <b>Tweak:LSQ fillin false</b>"));
   } else {
        GLwidget::setSplinefillin(false);
        GLwidget::setLSQfillin(true);
        LSQfillinAct->setChecked(GLwidget::isLSQfillin());
        SplinefillinAct->setChecked(GLwidget::isSplinefillin());
        ui.infoLabel->setText(tr("Set <b>Tweak:LSQ fillin true</b>"));
   };
   if (GLwidget::isRedraw()) {
       redraw();
   };
}

void MainWindow::tweakSplinefill()
{
   if (GLwidget::isSplinefillin()) {
        GLwidget::setSplinefillin(false);
        LSQfillinAct->setChecked(GLwidget::isLSQfillin());
        SplinefillinAct->setChecked(GLwidget::isSplinefillin());
        ui.infoLabel->setText(tr("Set <b>Tweak:Spline fillin false</b>"));
   } else {
        GLwidget::setSplinefillin(true);
        GLwidget::setLSQfillin(false);
        LSQfillinAct->setChecked(GLwidget::isLSQfillin());
        SplinefillinAct->setChecked(GLwidget::isSplinefillin());
        ui.infoLabel->setText(tr("Set <b>Tweak:Spline fillin true</b>"));
   };
   if (GLwidget::isRedraw()) {
       redraw();
   };
}

void MainWindow::tweaklsqvsspline()
{
    if (GLwidget::islsqvsspline()) {
        GLwidget::setlsqvsspline(false);
        lsqvssplineAct->setChecked(GLwidget::islsqvsspline());
        make2dsplineAct->setChecked(GLwidget::is2dspline());
    } else {
        GLwidget::set2dspline(false);
        GLwidget::setlsqvsspline(true);
        lsqvssplineAct->setChecked(GLwidget::islsqvsspline());
        make2dsplineAct->setChecked(GLwidget::is2dspline());
    };
    if (GLwidget::isRedraw()) {
        redraw();
    };
}

void MainWindow::tweak2dspline()
{
    if (GLwidget::is2dspline()) {
        GLwidget::set2dspline(false);
        lsqvssplineAct->setChecked(GLwidget::islsqvsspline());
        make2dsplineAct->setChecked(GLwidget::is2dspline());
    } else {
        GLwidget::set2dspline(true);
        GLwidget::setlsqvsspline(false);
        lsqvssplineAct->setChecked(GLwidget::islsqvsspline());
        make2dsplineAct->setChecked(GLwidget::is2dspline());
    };
    if (GLwidget::isRedraw()) {
        redraw();
    };
}

void MainWindow::tweakaxisymmetric()
{
    if (GLwidget::isaxisymmetric()) {
        GLwidget::setaxisymmetric(false);
        axisymmetricAct->setChecked(GLwidget::isaxisymmetric());
    } else {
        GLwidget::setaxisymmetric(true);
        axisymmetricAct->setChecked(GLwidget::isaxisymmetric());
    };
    if (GLwidget::isRedraw()) {
        redraw();
    };
}


void MainWindow::colorrgb2()
{
   if (GLwidget::isrgb2()) {
        GLwidget::setrgb2(false);
        GLwidget::setUSSfixed(true);
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:rgb2 false</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::setrgb2(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:rgb2 true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };   
}

void MainWindow::colorrgb5()
{
   if (GLwidget::isrgb5()) {
        GLwidget::setrgb5(false);
        GLwidget::setUSSfixed(true);
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:rgb5 false</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::setrgb5(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:rgb5 true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::colorhsbrgb()
{
   if (GLwidget::ishsbrgb()) {
        GLwidget::sethsbrgb(false);
        GLwidget::setUSSfixed(true);
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:Hue Sat Brightness Map false</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::sethsbrgb(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:Hue Sat Brightness Map true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::colorgplotpalette()
{
   if (GLwidget::isgplotpalette()) {
        GLwidget::setgplotpalette(false);
        GLwidget::setUSSfixed(true);
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:gplot palette false</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::setgplotpalette(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:gplot palette true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::colorUSSpalettefixed()
{
   if (GLwidget::isUSSfixed()) {
        GLwidget::setUSSfixed(true);    //cannot turn off without turning something else on
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:USS Palette fixed range is default, deselect by setting another</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::setUSSfixed(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:USS Palette fixed range true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::colorUSSpalette()
{
   if (GLwidget::isUSSpalette()) {
        GLwidget::setUSSpalette(false);
        GLwidget::setUSSfixed(true);
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:USS Palette false</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::setUSSpalette(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:USS Palette true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::colorPerceptualUniformfixed()
{
   if (GLwidget::isperceptualuniformfixed()) {
        GLwidget::setperceptualuniformfixed(false);
        GLwidget::setUSSfixed(true);
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:Perceptually Uniform Palette fixed range false</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::setperceptualuniformfixed(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:Perceptually Uniform Palette fixed range true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::colorPerceptualUniformpalette()
{
   if (GLwidget::isperceptualuniformpalette()) {
        GLwidget::setperceptualuniformpalette(false);
        GLwidget::setUSSfixed(true);
        USSfixedAct->setChecked(GLwidget::isUSSfixed());
        ui.infoLabel->setText(tr("Set <b>View:Perceptually Uniform Palette false</b>"));
   } else {
        GLwidget::setAllmapsfalse();
        GLwidget::setperceptualuniformpalette(true);
        checkmapsflags();
        ui.infoLabel->setText(tr("Set <b>View:View:Perceptually Uniform Palette true</b>"));
        if (GLwidget::isRedraw()) {
            redraw();
        };
   };
}

void MainWindow::about()
{
   // https://www.modernescpp.com/index.php/asynchronous-callable-wrappers
   static const unsigned int hwGuess= 4;
   unsigned int hw = std::thread::hardware_concurrency();
   unsigned int hw2 = QThread::idealThreadCount();
   unsigned int hwConcurr= (hw != 0)? hw : hwGuess;
   std::string t = std::to_string(hwConcurr);
   char const *n_char = t.c_str();
   if (hw != hw2) {ui.infoLabel->setText(tr("CPU Cores found by Kernunos seems inconsistent"));}
   else {ui.infoLabel->setText(tr("CPU Cores found by Kernunos: ")+n_char);}
   const char *glstring;
   QByteArray gl8 = glstring_global.toLocal8Bit();
   glstring = gl8.data();
   QString sglVer = "Kernunos runs on Qt and OpenGL.\nSee acknowledgements\n";
   sglVer += "\nCPU Cores found: ";
   sglVer += n_char;
   sglVer += glstring;
   QMessageBox::about(this, tr("About Kernunos"),sglVer);
}

void MainWindow::aboutQt()
{
   ui.infoLabel->setText(tr("Invoked <b>Help|About Qt</b>"));
}

void MainWindow::showDocumentation()
{
    assistant->showDocumentation("index.html");
}

void MainWindow::createActions()
{
   openAct = new QAction(tr("&Load..."), this);
   openAct->setShortcuts(QKeySequence::Open);
   openAct->setStatusTip(tr("Load an existing data file"));
   connect(openAct, &QAction::triggered, this, &MainWindow::open);

   testAct = new QAction(tr("&Test"), this);
   testAct->setShortcuts(QKeySequence::UnknownKey);
   testAct->setStatusTip(tr("Generate some fake data"));
   connect(testAct, &QAction::triggered, this, &MainWindow::test);

   compareAct = new QAction(tr("&Compare..."), this);
   compareAct->setStatusTip(tr("Compare to previous file"));
   compareAct->setEnabled(false);
   connect(compareAct, &QAction::triggered, this, &MainWindow::compare);

   decenterAct = new QAction(tr("&Decenter image"), this);
   connect(decenterAct, &QAction::triggered, this, &MainWindow::decenter);
   decenterAct->setEnabled(false);

   swapAct = new QAction(tr("&Swap..."), this);
   swapAct->setStatusTip(tr("Swap data wth previous file"));
   swapAct->setEnabled(false);
   connect(swapAct, &QAction::triggered, this, &MainWindow::swap);

   zernAct = new QAction(tr("&Compute Zernike Coefficients"), this);
   zernAct->setStatusTip(tr("Compute Zernike coefficients and maps"));
   zernAct->setEnabled(false);
   connect(zernAct, &QAction::triggered, this, &MainWindow::zerncompute);

   ShowZernAct = new QAction(tr("&Show Zernike Coefficients"), this);
   ShowZernAct->setStatusTip(tr("Show Zernike coefficients and maps"));
   ShowZernAct->setEnabled(false);
   connect(ShowZernAct, &QAction::triggered, this, &MainWindow::showzern);

   ply2binAct = new QAction(tr("Binary PLY format..."), this);
   ply2binAct->setStatusTip(tr("Write a binary PLY file"));
   ply2binAct->setEnabled(false);
   connect(ply2binAct, &QAction::triggered, this, &MainWindow::ply2bin);

   off2stlAct = new QAction(tr("STL (ASCII or binary) format..."), this);
   off2stlAct->setStatusTip(tr("Write an ASCII or binary STL file"));
   off2stlAct->setEnabled(false);
   connect(off2stlAct, &QAction::triggered, this, &MainWindow::off2stl);

   makeoffAct = new QAction(tr("OFF format..."), this);
   makeoffAct->setStatusTip(tr("Write an OFF file"));
   makeoffAct->setEnabled(false);
   connect(makeoffAct, &QAction::triggered, this, &MainWindow::makeoff);

   makeplyAct = new QAction(tr("PLY (ASCII) format..."), this);
   makeplyAct->setStatusTip(tr("Write an ASCII PLY file"));
   makeplyAct->setEnabled(false);
   connect(makeplyAct, &QAction::triggered, this, &MainWindow::makeply);

   importexportAct = new QAction(tr("Export with Assimp (multiple formats)... "), this);
   importexportAct->setStatusTip(tr("Export with Assimp (multiple formats)... "));
   importexportAct->setEnabled(false);
   connect(importexportAct, &QAction::triggered, this, &MainWindow::importexport);

   exitAct = new QAction(tr("E&xit"), this);
   exitAct->setShortcuts(QKeySequence::Quit);
   exitAct->setStatusTip(tr("Exit the application"));
   connect(exitAct, &QAction::triggered, this, &QWidget::close);
   connect(exitAct, &QAction::triggered, qApp, &QApplication::closeAllWindows);

   aboutAct = new QAction(tr("&About"), this);
   aboutAct->setStatusTip(tr("Show the application's About box"));
   connect(aboutAct, &QAction::triggered, this, &MainWindow::about);

   redrawAct = new QAction(tr("&Redraw"), this);
   redrawAct->setEnabled(false);
   connect(redrawAct, &QAction::triggered, this, &MainWindow::redraw);

   redrawOptionAct = new QAction(tr("&Auto Redraw"), this);
   redrawOptionAct->setEnabled(false);
   connect(redrawOptionAct, &QAction::triggered, this, &MainWindow::redrawOption);
   redrawOptionAct->setCheckable(true);
   redrawOptionAct->setChecked(GLwidget::isRedraw());

   lightAct = new QAction(tr("&Lighting"), this);
   lightAct->setStatusTip(tr("Change the lighting in the window"));
   connect(lightAct, &QAction::triggered, this, &MainWindow::light);
   lightAct->setCheckable(true);
   lightAct->setChecked(GLwidget::isLight());  //need this because can control from commandline

   normalAct = new QAction(tr("&Show Normals"), this);
   normalAct->setStatusTip(tr("Show the surface normals"));
   connect(normalAct, &QAction::triggered, this, &MainWindow::normal);
   normalAct->setCheckable(true);
   normalAct->setChecked(GLwidget::isNormal()); //need this because can control from commandline

   pupilAct = new QAction(tr("&Show Pupil"), this);
   pupilAct->setStatusTip(tr("Show pupil data if any"));
   connect(pupilAct, &QAction::triggered, this, &MainWindow::pupil);
   pupilAct->setCheckable(true);

   centernodeAct=new QAction(tr("&Create center node to force MinMax at origin"), this);
   centernodeAct->setCheckable(true);
   connect(centernodeAct, &QAction::triggered, this, &MainWindow::tweakcenterNode);

   adjustradiiAct=new QAction(tr("&Adjust meridional radii to force MinMax at origin"), this);
   adjustradiiAct->setCheckable(true);
   connect(adjustradiiAct, &QAction::triggered, this, &MainWindow::tweakadjustradii);

   cubicAct=new QAction(tr("&Use cubic integration instead of trapezoidal"), this);
   cubicAct->setCheckable(true);
   connect(cubicAct, &QAction::triggered, this, &MainWindow::tweakcubic);

   LSQfillinAct=new QAction(tr("&Fill in missing Atlas data by circumferential LSQ"), this);
   LSQfillinAct->setCheckable(true);
   connect(LSQfillinAct, &QAction::triggered, this, &MainWindow::tweakLSQfill);

   SplinefillinAct=new QAction(tr("&Fill in missing Atlas data by circumferential spline"), this);
   SplinefillinAct->setCheckable(true);
   connect(SplinefillinAct, &QAction::triggered, this, &MainWindow::tweakSplinefill);

   lsqvssplineAct=new QAction(tr("&Use LSQ instead of circumferential spline"), this);
   lsqvssplineAct->setCheckable(true);
   connect(lsqvssplineAct, &QAction::triggered, this, &MainWindow::tweaklsqvsspline);
   lsqvssplineAct->setChecked(GLwidget::islsqvsspline());  //check initially because default is true

   make2dsplineAct=new QAction(tr("&Use 2-D LSQ spline instead of 1Dx1D/LSQ"), this);
   make2dsplineAct->setCheckable(true);
   connect(make2dsplineAct, &QAction::triggered, this, &MainWindow::tweak2dspline);

   axisymmetricAct=new QAction(tr("&Impose Axisymmetry assumption in calculations"), this);
   axisymmetricAct->setCheckable(true);
   connect(axisymmetricAct, &QAction::triggered, this, &MainWindow::tweakaxisymmetric);
   axisymmetricAct->setChecked(GLwidget::isaxisymmetric());  //check initially because default is true

   liocAct = new QAction(tr("&Lines of Curvature"), this);
   liocAct->setStatusTip(tr("Show plot of lines of curvature"));
   liocAct->setEnabled(false);
   connect(liocAct, &QAction::triggered, this, &MainWindow::LinesofCurvature);

   centerAct = new QAction(tr("&Center deviations"), this);
   centerAct->setStatusTip(tr("Center deviations"));
   centerAct->setEnabled(false);
   connect(centerAct, &QAction::triggered, this, &MainWindow::center);

   ringsAct = new QAction(tr("&Show circumferential ring lsqfillin/splinefillin (Atlas only)"), this);
   ringsAct->setStatusTip(tr("Show circumferential ring lsqfillin/splinefillin (Atlas only"));
   ringsAct->setEnabled(false);
   connect(ringsAct, &QAction::triggered, this, &MainWindow::rings);

   consistencyAct = new QAction(tr("&Check spline consistency (Atlas only)"), this);
   consistencyAct->setStatusTip(tr("Check spline consistency (Atlas only)"));
   consistencyAct->setEnabled(true);
   consistencyAct->setCheckable(true);
   connect(consistencyAct, &QAction::triggered, this, &MainWindow::consistency);

   gnuplotAct = new QAction(tr("&Plots with Gnuplot Splot"), this);
   gnuplotAct->setStatusTip(tr("Plots with Gnuplot Splot"));
   gnuplotAct->setEnabled(false);
   connect(gnuplotAct, &QAction::triggered, this, &MainWindow::gnuplotsplot);

   aboutQtAct = new QAction(tr("About &Qt"), this);
   aboutQtAct->setStatusTip(tr("Show the Qt library's About box"));
   connect(aboutQtAct, &QAction::triggered, qApp, &QApplication::aboutQt);
   connect(aboutQtAct, &QAction::triggered, this, &MainWindow::aboutQt);

   HelpAct = new QAction(tr("Help Contents"), this);
   HelpAct->setStatusTip(tr("Shows some help"));
   HelpAct->setShortcut(QKeySequence::HelpContents);
   connect(HelpAct, &QAction::triggered, this, &MainWindow::showDocumentation);

   AxialAct=new QAction(tr("&Axial or Sagittal Power"), this);
   AxialAct->setCheckable(true);
   connect(AxialAct, &QAction::triggered, this, &MainWindow::fctAxial);
   AxialAct->setChecked(GLwidget::isAxial());  //needs this here to check initially because it is the default

   ObliqueAct=new QAction(tr("&Warp or Oblique Power, explicitly non-meridional"), this);
   ObliqueAct->setCheckable(true);
   connect(ObliqueAct, &QAction::triggered, this, &MainWindow::fctOblique);

   TangentialAct=new QAction(tr("&Tangential or Instantaneous Power"), this);
   TangentialAct->setCheckable(true);
   connect(TangentialAct, &QAction::triggered, this, &MainWindow::fctTangential);

   GaussianAct=new QAction(tr("&Gaussian Power"), this);
   GaussianAct->setCheckable(true);
   connect(GaussianAct, &QAction::triggered, this, &MainWindow::fctGaussian);

   MeanAct=new QAction(tr("&Mean Power"), this);
   MeanAct->setCheckable(true);
   connect(MeanAct, &QAction::triggered, this, &MainWindow::fctMean);

   AstigAct=new QAction(tr("&Monge Astigmatism"), this);
   AstigAct->setCheckable(true);
   connect(AstigAct, &QAction::triggered, this, &MainWindow::fctMongeAstig);

   ElevationAct=new QAction(tr("&Elevation"), this);
   ElevationAct->setCheckable(true);
   connect(ElevationAct, &QAction::triggered, this, &MainWindow::fctElevation);

   Z44VerticalQuatrafoilAct=new QAction(tr("&Vertical Quatrafoil"), this);
   Z44VerticalQuatrafoilAct->setCheckable(true);
   Z44VerticalQuatrafoilAct->setEnabled(false);
   connect(Z44VerticalQuatrafoilAct, &QAction::triggered, this, &MainWindow::fctZ44);

   Z42Vertical2ndAstigAct=new QAction(tr("&Vertical 2nd Astigmatism"), this);
   Z42Vertical2ndAstigAct->setCheckable(true);
   Z42Vertical2ndAstigAct->setEnabled(false);
   connect(Z42Vertical2ndAstigAct, &QAction::triggered, this, &MainWindow::fctZ42);

   Z40SphericalAberrationAct=new QAction(tr("&Spherical Aberration"), this);
   Z40SphericalAberrationAct->setCheckable(true);
   Z40SphericalAberrationAct->setEnabled(false);
   connect(Z40SphericalAberrationAct, &QAction::triggered, this, &MainWindow::fctZ40);

   Z4neg2Oblique2ndAstigAct=new QAction(tr("&Oblique 2nd Astigmatism"), this);
   Z4neg2Oblique2ndAstigAct->setCheckable(true);
   Z4neg2Oblique2ndAstigAct->setEnabled(false);
   connect(Z4neg2Oblique2ndAstigAct, &QAction::triggered, this, &MainWindow::fctZ4neg2);

   Z4neg4ObliqueQuatrafoilAct=new QAction(tr("&Oblique Quadrafoil"), this);
   Z4neg4ObliqueQuatrafoilAct->setCheckable(true);
   Z4neg4ObliqueQuatrafoilAct->setEnabled(false);
   connect(Z4neg4ObliqueQuatrafoilAct, &QAction::triggered, this, &MainWindow::fctZ4neg4);

   Z33ObliqueTrefoilAct=new QAction(tr("&Oblique Trefoil"), this);
   Z33ObliqueTrefoilAct->setCheckable(true);
   Z33ObliqueTrefoilAct->setEnabled(false);
   connect(Z33ObliqueTrefoilAct, &QAction::triggered, this, &MainWindow::fctZ33);

   Z3neg3VerticalTrefoilAct=new QAction(tr("&Vertical Trefoil"), this);
   Z3neg3VerticalTrefoilAct->setCheckable(true);
   Z3neg3VerticalTrefoilAct->setEnabled(false);
   connect(Z3neg3VerticalTrefoilAct, &QAction::triggered, this, &MainWindow::fctZ3neg3);

   Z31HorizontalComaAct=new QAction(tr("&Horizontal Coma"), this);
   Z31HorizontalComaAct->setCheckable(true);
   Z31HorizontalComaAct->setEnabled(false);
   connect(Z31HorizontalComaAct, &QAction::triggered, this, &MainWindow::fctZ31);

   Z3neg1VerticalComaAct=new QAction(tr("&Vertical Coma"), this);
   Z3neg1VerticalComaAct->setCheckable(true);
   Z3neg1VerticalComaAct->setEnabled(false);
   connect(Z3neg1VerticalComaAct, &QAction::triggered, this, &MainWindow::fctZ3neg1);

   Z22VerticalAstigAct=new QAction(tr("&Vertical Astigmatism"), this);
   Z22VerticalAstigAct->setCheckable(true);
   Z22VerticalAstigAct->setEnabled(false);
   connect(Z22VerticalAstigAct, &QAction::triggered, this, &MainWindow::fctZ22);

   Z2neg2ObliqueAstigAct=new QAction(tr("&Oblique Astigmatism"), this);
   Z2neg2ObliqueAstigAct->setCheckable(true);
   Z2neg2ObliqueAstigAct->setEnabled(false);
   connect(Z2neg2ObliqueAstigAct, &QAction::triggered, this, &MainWindow::fctZ2neg2);

   Z20DefocusAct=new QAction(tr("&Defocus"), this);
   Z20DefocusAct->setCheckable(true);
   Z20DefocusAct->setEnabled(false);
   connect(Z20DefocusAct, &QAction::triggered, this, &MainWindow::fctZ20);

   Z11XtiltAct=new QAction(tr("&X-Tilt"), this);
   Z11XtiltAct->setCheckable(true);
   Z11XtiltAct->setEnabled(false);
   connect(Z11XtiltAct, &QAction::triggered, this, &MainWindow::fctZ11);

   Z1neg1YtiltAct=new QAction(tr("&Y-Tilt"), this);
   Z1neg1YtiltAct->setCheckable(true);
   Z1neg1YtiltAct->setEnabled(false);
   connect(Z1neg1YtiltAct, &QAction::triggered, this, &MainWindow::fctZ1neg1);

   Z00PistonAct=new QAction(tr("&Piston"), this);
   Z00PistonAct->setCheckable(true);
   Z00PistonAct->setEnabled(false);
   connect(Z00PistonAct, &QAction::triggered, this, &MainWindow::fctZ00);

   rgb2Act=new QAction(tr("&Discrete 2 color Heatmap with linear interpolation"), this);
   rgb2Act->setCheckable(true);
   connect(rgb2Act, &QAction::triggered, this, &MainWindow::colorrgb2);

   rgb5Act=new QAction(tr("&Discrete 5 color Heatmap with linear interpolation"), this);
   rgb5Act->setCheckable(true);
   connect(rgb5Act, &QAction::triggered, this, &MainWindow::colorrgb5);

   hsbrgbAct=new QAction(tr("&Continuous Hue Heatmap with fixed Saturation and Brightness"), this);
   hsbrgbAct->setCheckable(true);
   connect(hsbrgbAct, &QAction::triggered, this, &MainWindow::colorhsbrgb);

   gplotpaletteAct=new QAction(tr("&Discrete 12 color Heatmap no interpolation gnuplot style"), this);
   gplotpaletteAct->setCheckable(true);
   connect(gplotpaletteAct, &QAction::triggered, this, &MainWindow::colorgplotpalette);

   USSfixedAct=new QAction(tr("&Uniform Standard Scale (Smolek-Klyce) discrete map with linear interpolation between 26 colors with fixed range for sagittal/axial powers"), this);
   USSfixedAct->setCheckable(true);
   connect(USSfixedAct, &QAction::triggered, this, &MainWindow::colorUSSpalettefixed);
   USSfixedAct->setChecked(GLwidget::isUSSfixed());  //needs this here to check initially because it is the default

   USSPaletteAct=new QAction(tr("&Uniform Standard Scale (Smolek-Klyce) discrete map with linear interpolation between 26 colors"), this);
   USSPaletteAct->setCheckable(true);
   connect(USSPaletteAct, &QAction::triggered, this, &MainWindow::colorUSSpalette);

   PerceptualfixedAct=new QAction(tr("&Perceptually Uniform 9 shade Palette discrete map with linear interpolation with ANSI Z80.3 fixed range"), this);
   PerceptualfixedAct->setCheckable(true);
   connect(PerceptualfixedAct, &QAction::triggered, this, &MainWindow::colorPerceptualUniformfixed);

   PerceptuallyUniformPaletteAct=new QAction(tr("&Perceptually Uniform 9 shade palette discrete map with linear interpolation"), this);
   PerceptuallyUniformPaletteAct->setCheckable(true);
   connect(PerceptuallyUniformPaletteAct, &QAction::triggered, this, &MainWindow::colorPerceptualUniformpalette);
}

void MainWindow::createMenus()
{
   fileMenu = menuBar()->addMenu(tr("&File"));
   fileMenu->addAction(openAct);
   fileMenu->addAction(testAct);
   fileMenu->addAction(consistencyAct);
   fileMenu->addAction(compareAct);
   fileMenu->addAction(decenterAct);
   fileMenu->addAction(swapAct);
   exportMenu = fileMenu->addMenu(tr("&Export"));
   exportMenu->addAction(makeplyAct);
   exportMenu->addAction(ply2binAct);
   exportMenu->addAction(makeoffAct);
   exportMenu->addAction(off2stlAct);
   exportMenu->addAction(importexportAct);
   fileMenu->addSeparator();
   fileMenu->addAction(exitAct);
   menuBar()->addAction(redrawAct);
   analyzeMenu = menuBar()->addMenu(tr("&Analyze"));
   analyzeMenu->addAction(zernAct);
   analyzeMenu->addAction(ShowZernAct);
   analyzeMenu->addAction(liocAct);
   analyzeMenu->addAction(centerAct);
   analyzeMenu->addAction(ringsAct);
   analyzeMenu->addAction(gnuplotAct);
   functionMenu=menuBar()->addMenu(tr("&Function"));
   functionMenu->addAction(AxialAct);
   functionMenu->addAction(ObliqueAct);
   functionMenu->addAction(TangentialAct);
   functionMenu->addAction(GaussianAct);
   functionMenu->addAction(MeanAct);
   functionMenu->addAction(AstigAct);
   functionMenu->addAction(ElevationAct);
   functionMenu->addAction(Z44VerticalQuatrafoilAct);
   functionMenu->addAction(Z42Vertical2ndAstigAct);
   functionMenu->addAction(Z40SphericalAberrationAct);
   functionMenu->addAction(Z4neg2Oblique2ndAstigAct);
   functionMenu->addAction(Z4neg4ObliqueQuatrafoilAct);
   functionMenu->addAction(Z33ObliqueTrefoilAct);
   functionMenu->addAction(Z3neg3VerticalTrefoilAct);
   functionMenu->addAction(Z31HorizontalComaAct);
   functionMenu->addAction(Z3neg1VerticalComaAct);
   functionMenu->addAction(Z22VerticalAstigAct);
   functionMenu->addAction(Z2neg2ObliqueAstigAct);
   functionMenu->addAction(Z20DefocusAct);
   functionMenu->addAction(Z11XtiltAct);
   functionMenu->addAction(Z1neg1YtiltAct);
   functionMenu->addAction(Z00PistonAct);
   colorMenu=menuBar()->addMenu(tr("&Color"));
   colorMenu->addAction(rgb2Act);
   colorMenu->addAction(rgb5Act);
   colorMenu->addAction(hsbrgbAct);
   colorMenu->addAction(gplotpaletteAct);
   colorMenu->addAction(USSfixedAct);
   colorMenu->addAction(USSPaletteAct);
   colorMenu->addAction(PerceptualfixedAct);
   colorMenu->addAction(PerceptuallyUniformPaletteAct);
   viewMenu = menuBar()->addMenu(tr("&View"));
   viewMenu->addAction(redrawOptionAct);
   viewMenu->addAction(lightAct);
   viewMenu->addAction(normalAct);
   viewMenu->addAction(pupilAct);
   tweaksMenu = menuBar()->addMenu(tr("&Placido data tweaks"));
   tweaksMenu->addAction(axisymmetricAct);
   tweaksMenu->addAction(centernodeAct);
   tweaksMenu->addAction(adjustradiiAct);
   tweaksMenu->addAction(cubicAct);
   tweaksMenu->addAction(LSQfillinAct);
   tweaksMenu->addAction(SplinefillinAct);
   tweaksMenu->addAction(lsqvssplineAct);
   tweaksMenu->addAction(make2dsplineAct);
   helpMenu = menuBar()->addMenu(tr("&About"));
   helpMenu->addAction(HelpAct);
   helpMenu->addAction(aboutAct);
   helpMenu->addAction(aboutQtAct);
}

void MainWindow::updateResult()
{
   ui.progressBar->setValue(counter);

 // Legend colorscale and numbers, updated
    int scale = 680;
    int scale2 = 30;
    QLinearGradient grBtoY(0, 0, 1, scale);
    QLinearGradient grBtoY2(0, 0, 1, scale);
    for (int i = 1; i <= 26; ++i) {
        QColor rgbcolor = QColor::fromRgb(legend[(i-1)*4+1],
                                         legend[(i-1)*4+2],
                                         legend[(i-1)*4+3],
                                          255);
        QColor rgbcolor2 = QColor::fromRgb(legend2[(i-1)*4+1],
                                          legend2[(i-1)*4+2],
                                          legend2[(i-1)*4+3],
                                          255);
/*
    // write the values
        std::cout << "legend[" << (i-1)*4+1 << "]=" <<
                legend2[(i-1)*4+1] << ";\n" << "legend[" << (i-1)*4+2 << "]=" <<
            legend2[(i-1)*4+2] << ";\n" << "legend[" << (i-1)*4+3 << "]=" <<
            legend2[(i-1)*4+3] << ";\n" << "legend[" <<(i-1)*4+0 << "]=" <<
            floor(legend2[(i-1)*4]+0.5) << ";\n" <<
            std::endl;
*/

        grBtoY.setColorAt((i-1)/26.0, rgbcolor);
        grBtoY2.setColorAt((i-1)/26.0, rgbcolor2);
    }
    QPixmap pm(scale2, scale);
    QPainter pmp(&pm);
    pmp.setBrush(QBrush(grBtoY));
    pmp.setPen(Qt::NoPen);
    pmp.setRenderHint(QPainter::Antialiasing, true);
    QRect rect1(0, 0, scale2, scale);
    pmp.drawRect(rect1);
    ui.legendpix->setPixmap(pm);
    QPixmap pm2(scale2, scale);
    QPainter pmp2(&pm2);
    pmp2.setBrush(QBrush(grBtoY2));
    pmp2.setPen(Qt::NoPen);
    pmp2.setRenderHint(QPainter::Antialiasing, true);
    pmp2.drawRect(rect1);
    ui.legendpix_2->setPixmap(pm2);
    QString legendvalues = "";
    for (int i = 1; i <= 13; ++i) {
        float j = floor(legend[(i-1)*8]+0.5);
        std::string t = std::to_string(j);  //stuck with 6 digits output
        char const *n_char = t.c_str();
        legendvalues += "\n";
        legendvalues += n_char;
        legendvalues += "\n";
        legendvalues += "\n";
    }
    QString legendvalues2 = "";
    for (int i = 1; i <= 13; ++i) {
        float j = floor(legend2[(i-1)*8]+0.5);
        std::string t2 = std::to_string(j);  //stuck with 6 digits output
        char const *n_char2 = t2.c_str();
        legendvalues2 += "\n";
        legendvalues2 += n_char2;
        legendvalues2 += "\n";
        legendvalues2 += "\n";
    }
    ui.legend->setText(legendvalues);
    ui.legend->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    ui.legendpix->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    ui.legend_2->setText(legendvalues2);
    ui.legend_2->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    ui.legendpix_2->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

}

int main(int argc, char *argv[])
{
   QApplication::setStyle(QStyleFactory::create("fusion"));
   QApplication app(argc, argv);
   QCoreApplication::setOrganizationName("QtProject");
   QCoreApplication::setApplicationName("kernunos");
   QCoreApplication::setApplicationVersion(QT_VERSION_STR);
   QCommandLineParser parser;
   parser.setApplicationDescription(QCoreApplication::applicationName());
   parser.addHelpOption();
   parser.addVersionOption();
   parser.addPositionalArgument("file", "The file to open.");

   QCommandLineOption multipleSampleOption("multisample", "Multisampling");
   parser.addOption(multipleSampleOption);
   QCommandLineOption transparentOption("transparent", "Transparent window");
   parser.addOption(transparentOption);
   QCommandLineOption normalsOption("normals", "show surface normals ");
   parser.addOption(normalsOption);

   parser.process(app);

   QSurfaceFormat fmt;
   fmt.setDepthBufferSize(24);
   if (parser.isSet(multipleSampleOption))
       fmt.setSamples(4);
   QSurfaceFormat::setDefaultFormat(fmt);

   QTranslator translator;
   const QStringList uiLanguages = QLocale::system().uiLanguages();
   for (const QString &locale : uiLanguages) {
       const QString baseName = "untitled_" + QLocale(locale).name();
       if (translator.load(":/i18n/" + baseName)) {
            app.installTranslator(&translator);
            break;
        }
    }

    MainWindow window;

    if (!parser.positionalArguments().isEmpty())
    {window.loadFile(parser.positionalArguments().first(),true);}  //uses loadfile() above to load a commandline filename
    else
     {QString fileName="cube";
      window.loadFile(fileName,false); }

    GLwidget::setNormal(parser.isSet(normalsOption));
    GLwidget::setTransparent(parser.isSet(transparentOption));
    if (GLwidget::isTransparent()) {
        window.setAttribute(Qt::WA_TranslucentBackground);
        window.setAttribute(Qt::WA_NoSystemBackground, false);
    }

//  logs a comment to kernunos.log
    LogC("open a log file");

//  logs the stdout
    FILE *fp;
    fp = freopen( "logstdout.log", "w", stdout );

    window.resize(window.sizeHint());
    int desktopArea = QGuiApplication::primaryScreen()->size().width() *
                      QGuiApplication::primaryScreen()->size().height();
    int widgetArea = window.width() * window.height();
    if (((float)widgetArea / (float)desktopArea) < 0.75f){
    window.show();
    window.grabGesture(Qt::PanGesture);
    window.grabGesture(Qt::PinchGesture);
    }
    else {
    window.showMaximized();
    window.grabGesture(Qt::PanGesture);
    window.grabGesture(Qt::PinchGesture);
    }
    return app.exec();
    fclose(fp);
}

