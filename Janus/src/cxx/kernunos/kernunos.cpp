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

//not found for some reason, need explicit location here
//https://community.intel.com/t5/Intel-Fortran-Compiler/How-to-access-ALLOCATABLE-4-D-Fortran-array-from-C/m-p/1478884/highlight/true
//#include "/usr/lib/gcc/x86_64-pc-linux-gnu/13.2.1/include/ISO_Fortran_binding.h"

//lifted from examples zoomlinechart
#include "chart.h"   // Copyright (C) 2023 The Qt Company Ltd.
#include "chartview.h" // SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause
#include "window.h"
#include <QtWidgets>
#include <QtConcurrent>
#include <functional>
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

#include <assimp/cimport.h>
#include <assimp/Importer.hpp>
#include <assimp/Exporter.hpp>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

#include "/home/debeus/Janus/Janus/src/c/kernunos/logc.h"

using namespace QtConcurrent;

// global settings
const unsigned int SCR_WIDTH = 1500;
const unsigned int SCR_HEIGHT = 800;

int flag=0;

//https://stackoverflow.com/questions/16296284/workaround-for-blocking-async
// was const char *filename and not global

char *filename;
//char *message;

bool success=false;
bool paintme = false;

//how very Fortran that these need to be static & global
int nV;
int nE;
std::vector<GLuint> Elements(26130);
std::vector<GLfloat> Vertices(51840);
GLfloat* vertices = Vertices.data();
GLuint* elements = Elements.data();

QString *m_GLString=nullptr;
QString glstring_global;

/*
bool LogCreated = false;
extern void LogC (QString Message) { FILE *file;
    if (!LogCreated) { file = fopen(LOGFILE, "w");
        LogCreated = true; }
    else file = fopen(LOGFILE, "a");
    if (file == NULL) { if (LogCreated) LogCreated = false; return; }
    else {
        QByteArray ba = Message.toLocal8Bit();
        message = ba.data();
        fprintf(file, "%s", message);
        fprintf(file,"\n");
        fclose(file); }
    }
*/
static QMainWindow *findMainWindow()
{
    for (auto *w : QApplication::topLevelWidgets()) {
        if (auto *mw = qobject_cast<QMainWindow *>(w))
            return mw;
    }
    return nullptr;
}

MainWindow::MainWindow()
{
   QWidget *widget = new QWidget;
   widget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

   ui.setupUi(this);
   connect(ui.inputSpinBox1, &QSpinBox::valueChanged, this, &MainWindow::updateResult);
   connect(ui.inputSpinBox2, &QSpinBox::valueChanged, this, &MainWindow::updateResult);

   ui.outputWidget->setText("Sum");
   ui.progressBar->setValue(0);

   ui.infoLabel->setText(tr("<i>Welcome! Please Open a file.</i>"));
   ui.infoLabel->setFrameStyle(QFrame::StyledPanel | QFrame::Sunken);
   ui.infoLabel->setAlignment(Qt::AlignCenter);

   QVBoxLayout *vlayout = new QVBoxLayout();

   QGroupBox *colorGroupBox = new QGroupBox(QStringLiteral("First Window"));
   QLinearGradient grBtoY(0, 0, 1, 600);
   // from rgb5color {{0,0,255},{0,255,255},{0,255,0},{255,255,0},{255,0,0}}
   QColor rgbcolor1= QColor::fromRgb(0, 0, 255, 255);
   grBtoY.setColorAt(1.0, rgbcolor1);
   QColor rgbcolor2= QColor::fromRgb(0, 255, 255, 255);
   grBtoY.setColorAt(0.67, rgbcolor2);
   QColor rgbcolor3= QColor::fromRgb(0, 255, 0, 255);
   grBtoY.setColorAt(0.33, rgbcolor3);
   QColor rgbcolor4= QColor::fromRgb(255, 255, 0, 255);
   grBtoY.setColorAt(0.0, rgbcolor4);
   QColor rgbcolor5= QColor::fromRgb(255, 0, 0, 255);
   grBtoY.setColorAt(0.0, rgbcolor5);
   QPixmap pm(24, 600);
   QPainter pmp(&pm);
   pmp.setBrush(QBrush(grBtoY));
   pmp.setPen(Qt::NoPen);
   pmp.setRenderHint(QPainter::Antialiasing, true);

   QRect rect1(0, 0, 24, 600);  //there are three 600s here that need auto resize
   pmp.drawRect(rect1);

//    QLabel *legendpix = new QLabel(widget);
//    QLabel *legend = new QLabel(widget);

    ui.legendpix->setPixmap(pm);
    QString legendvalues = "";
    for (int i = 1; i <= 37; ++i) {
     int j = 60;
     j=j-i*1.0;
     std::string t = std::to_string(j);
     char const *n_char = t.c_str();
     legendvalues += n_char;
     legendvalues += "\n";
    }

    ui.legend->setText(legendvalues);
    ui.legend->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    ui.legendpix->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QHBoxLayout *colorHBox = new QHBoxLayout;

    //colorHBox->addWidget(legendpix);
   // colorHBox->addWidget(legend);
    colorGroupBox->setLayout(colorHBox);

   QBarSet *negative = new QBarSet("Negative");
   QBarSet *positive = new QBarSet("Positive");

   *negative << -.42 << 0 << -.45 << -.37 << -.25 << -0.08
             << -0.0 << -0 << 0 << 0 << -0 << -0.1;
   *positive << 0  << .128 << 0 << 0 << 0 << 0
             << .38 << .34 << .29 << .204 << .15 << 0;

   QHorizontalStackedBarSeries *series = new QHorizontalStackedBarSeries();

   series->append(negative);
   negative->setColor(QColorConstants::Red);
   series->append(positive);
   positive->setColor(QColorConstants::Blue);

   // Uses zoomlinechart example Chart class
   QChart *chart = new Chart();
   chart->addSeries(series);
   chart->setTitle("Corneal Aberrometry");
   chart->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

   QStringList aberrations = {
       "Z(4,4) Vertical Quatrafoil",
       "Z(4,2) Vertical 2nd Astig.",
       "Z(4,0) Spherical Aberration",
       "Z(4,-2) Oblique 2nd Astig.",
       "Z(4,-4) Oblique Quatrafoil",
       "Z(3,3) Oblique Trefoil",
       "Z(3,1) Horizontal Coma",
       "Z(3,-1) Vertical Coma",
       "Z(3,-3) Vertical Trefoil",
       "Z(2,2) Vertical Astig.",
       "Z(2,0) Defocus",
       "Z(2,-2) Oblique Astigmatism"
   };

   QValueAxis *axisX = new QValueAxis();
   QBarCategoryAxis *axisY = new QBarCategoryAxis();
   axisY->append(aberrations);

   axisX->setRange(-0.500, 0.500);
   axisX->setTitleText("micrometers");

   chart->addAxis(axisX, Qt::AlignBottom);
   chart->addAxis(axisY, Qt::AlignRight);
   series->attachAxis(axisX);
   series->attachAxis(axisY);

   chart->legend()->setVisible(false);
   chart->legend()->setAlignment(Qt::AlignBottom);
   chart->setAnimationOptions(QChart::SeriesAnimations);

   // uses zoomlinechart example ChartView class
   QChartView *chartView = new ChartView(chart);
   chartView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
   chartView->setRenderHint(QPainter::Antialiasing);
   chartView->setMinimumSize(400,400);   //another hard code number!


/* if using ui, have to edit properties as below
   slider->setRange(0, 360 * 16);
   slider->setSingleStep(16);
   slider->setPageStep(15 * 16);
   slider->setTickInterval(15 * 16);
   slider->setTickPosition(QSlider::TicksRight);*/
   connect(ui.xSlider, &QSlider::valueChanged, ui.openGLWidget_2, &GLwidget::setXRotation);
   connect(ui.openGLWidget_2, &GLwidget::xRotationChanged, ui.xSlider, &QSlider::setValue);
   connect(ui.ySlider, &QSlider::valueChanged, ui.openGLWidget_2, &GLwidget::setYRotation);
   connect(ui.openGLWidget_2, &GLwidget::yRotationChanged, ui.ySlider, &QSlider::setValue);
   connect(ui.zSlider, &QSlider::valueChanged, ui.openGLWidget_2, &GLwidget::setZRotation);
   connect(ui.openGLWidget_2, &GLwidget::zRotationChanged, ui.zSlider, &QSlider::setValue);

   connect(ui.dockButton, &QPushButton::clicked, this, &MainWindow::dockUndock);


   colorHBox->addWidget(chartView);
   vlayout->addWidget(colorGroupBox);
   widget->setLayout(vlayout);

   ui.xSlider->setValue(0 * 16);
   ui.ySlider->setValue(345 * 16);
   ui.zSlider->setValue(15 * 16);

   QGraphicsScene *scene = new QGraphicsScene();
   ui.graphicsView->setScene(scene);
   scene->addWidget(widget);

   QGraphicsScene *scene2 = new QGraphicsScene();
   ui.graphicsView_2->setScene(scene2);
   widget=new Window;
   scene2->addWidget(widget);

   //onAddNew();

   createActions();
   createMenus();
   setWindowTitle(tr("Kernunos"));
   setMinimumSize(400, 400);
   resize(SCR_WIDTH, SCR_HEIGHT);
   update();

}

void MainWindow::dockUndock()
{
   if (parent()){
     undock();
     std::cout <<"undock";
   }
   else {
     dock();
     std::cout <<"dock";
   }
}

void MainWindow::dock()
{
   auto *mainWindow = findMainWindow();

   if (mainWindow == nullptr || !mainWindow->isVisible()) {
     QMessageBox::information(this, tr("Cannot Dock Closed"),
                              tr("Main window already closed"));
     return;
   }
   if (mainWindow->centralWidget()) {
     QMessageBox::information(this, tr("Cannot Dock Occupied"),
                              tr("Main window already occupied"));
     return;
   }
   setAttribute(Qt::WA_DeleteOnClose, false);
   ui.dockButton->setText(tr("Undock"));
   mainWindow->setCentralWidget(this);
   show();

}

void MainWindow::undock()
{
   setParent(nullptr);
   setAttribute(Qt::WA_DeleteOnClose);
   const auto geometry = screen()->availableGeometry();
   move(geometry.x() + (geometry.width() - width()) / 2,
        geometry.y() + (geometry.height() - height()) / 2);
   ui.dockButton->setText(tr("Dock"));
   show();
}


void MainWindow::SetGLString(QString& gls)
{    m_GLString =  &gls;
   glstring_global=*m_GLString;
}

void MainWindow::open()   //multiple invocations makes a comparison
{
   ui.infoLabel->setText(tr("Invoked <b>File|Open</b>"));

   QString filter = "All (*.*);;PentaCam (*.CUR *.ELE *.CUR.CSV *.ELE.CSV);;EyeSys (*.DAT);;Atlas (*.CSV)";
   QString fileName = QFileDialog::getOpenFileName(this,"Open a file", "", filter);
   if (fileName.isEmpty())
       return;
   QByteArray ba = fileName.toLocal8Bit();
   filename = ba.data();
   ui.infoLabel->setText(tr("filename:  ")+tr(filename));
   if (!fileName.isEmpty())
       m_GLwidget->DataLoad(fileName, false);
   update();
}

void MainWindow::loadFile(QString& fileName, bool filepresent)   //this is for the commandline file if any
{
   if (fileName.isEmpty())
       return;
   QByteArray ba = fileName.toLocal8Bit();
   //const char *filename = ba.data();
   filename = ba.data();
   ui.infoLabel->setText(tr("filename:  ")+tr(filename));
   //if (!fileName.isEmpty())
   if (filepresent){
       m_GLwidget->DataLoad(fileName, false);}
   else {m_GLwidget->DataLoad(fileName, true);}  //cube
   update();
}

void MainWindow::compare()    //right now this foesn't do anything but direct output to second_window (which means nothing as there's only one buffer) and make a cube.
{
   ui.infoLabel->setText(tr("Invoked <b>File|Compare</b>"));

   QString filter = "All (*.*);;PentaCam (*.CUR *.ELE *.CUR.CSV *.ELE.CSV);;EyeSys (*.DAT);;Atlas (*.CSV)";
   QString fileName = QFileDialog::getOpenFileName(this,"Open a file", "", filter);
   if (fileName.isEmpty())
       return;
   QByteArray ba = fileName.toLocal8Bit();
   const char *filename = ba.data();
   ui.infoLabel->setText(tr("filename:  ")+tr(filename));
   if (!fileName.isEmpty())
       m_GLwidget_secondwindow->DataLoad(fileName,true);
   update();
}

void MainWindow::zern()
{
   QTemporaryFile file;
    QString fileName = file.fileName();
       flag=1;
       m_GLwidget_secondwindow->DataPrint(fileName);
   update();
/*
    const int iterations = 20;
    // Prepare the vector.
    QVector<int> vector;
    for (int i = 0; i < iterations; ++i)
        vector.append(i);

    // Create a progress dialog.
    QProgressDialog dialog;
    dialog.setLabelText(QString("Progressing using %1 thread(s)...").arg(QThread::idealThreadCount()));

    // Create a QFutureWatcher and connect signals and slots.
    QFutureWatcher<void> futureWatcher;
    QObject::connect(&futureWatcher, &QFutureWatcher<void>::finished, &dialog, &QProgressDialog::reset);
    QObject::connect(&dialog, &QProgressDialog::canceled, &futureWatcher, &QFutureWatcher<void>::cancel);
    QObject::connect(&futureWatcher,  &QFutureWatcher<void>::progressRangeChanged, &dialog, &QProgressDialog::setRange);
    QObject::connect(&futureWatcher, &QFutureWatcher<void>::progressValueChanged,  &dialog, &QProgressDialog::setValue);

    // Our function to compute
    std::function<void(int&)> spin = [](int &iteration) {
        const int work = 1000 * 1000 * 400;
        volatile int v = 0;
        for (int j = 0; j < work; ++j)
            ++v;

        qDebug() << "iteration" << iteration << "in thread" << QThread::currentThreadId();
    };

    // Start the computation.
    futureWatcher.setFuture(QtConcurrent::map(vector, spin));


    // Display the dialog and start the event loop.
    dialog.exec();

    futureWatcher.waitForFinished();

    // Query the future to check if was canceled.
    qDebug() << "Canceled?" << futureWatcher.future().isCanceled();

*/
   ui.infoLabel->setText(tr("Invoked <b>File|Save</b>"));
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
   const char *filename = ba.data();
// generate temp ply file
   flag=3;
   m_GLwidget_secondwindow->DataPrint(filename);
// get output file name and type
   QString filter =
       "Stanford Polygon Library ASCII .ply (*.ply) ;; "
       "Stereolithography .stl (*.stl) ;; "
       "Stereolithography binary .stlb (*.stlb) ;; "
       "Extensible 3D .x3d (*.x3d) ;; "
       "Direct3D XFile .x (*.x) ;; "
       "Autodesk FBX .fbx (*.fbx) ;; "
       "Wavefront Object .obj (*.obj) ;; "
       "Collada .dae (*.dae) ;; "
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
   // only obj,dae,ascii ply,binary and ascii stl,3ds,x and fbx verified to work from ply. Colors are not always preserved eg. stl
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
       std::cout << "id: "<< i << " " << iformat->mFileExtensions << "\n";
       i++;
   } while (i < countin);
*/
   //std::cout << "ID: "<< ID << "\n";
   iformat = Importer.GetImporterInfo(ID);
   uint count = Exporter.GetExportFormatCount();
   // std::cout << "count: "<< count << "\n";
   i = 0 ;
   do {
       format = Exporter.GetExportFormatDescription(i);
    //   std::cout << "id: "<< i << " " << format->id << "\n";
       if (iformat->mFileExtensions == format->id){
            //        std::cout << "ID is " << i << "\n";
            Exporter.Export(aiscene, format->id , filenameout, 0);
            std::cout << "Wrote " << filenameout << "\n";
       }
       i++;
   } while (i < count);
   // special cases that have mismatched import and export descriptions
   if (ID == 26) {  // dae or collada
       format = Exporter.GetExportFormatDescription(0);
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
   }
   if (ID == 45) {  //x3d
       format = Exporter.GetExportFormatDescription(16);
       Exporter.Export(aiscene, format->id , filenameout, 0);
       std::cout << "Wrote " << filenameout << "\n";
   }
   if (ID == 3) { //3ds
       format = Exporter.GetExportFormatDescription(9);
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
    const char *filenameout = ba.data();
    //make temporary PLY file
    /*
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
    const char *filename = ba.data();
    flag=3;
       m_GLwidget_secondwindow->DataPrint(filename);
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
   QString filter = "STL *.stl  (*.stl) ;; Binary *.bin.stl (*.bin.stl)";
   QString fileName = QFileDialog::getSaveFileName(this,"Write to an ASCII STL or Binary STL file", "", filter);
   if (fileName.isEmpty())
      return;
   QByteArray ba = fileName.toLocal8Bit();
   char *filenameout = ba.data();
   //make temporary OFF file
   /*
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
   char *filename = ba.data();
   flag=2;
   m_GLwidget_secondwindow->DataPrint(filename);
   ConvertOFFtoSTL_C_(filename,filenameout);
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
   char *filenameout = ba.data();
   flag=2;
   m_GLwidget_secondwindow->DataPrint(filenameout);
   ui.infoLabel->setText(tr("Wrote  ")+tr(filenameout));
}

void MainWindow::makeply()
{
   QString filter = "PLY *.ply  (*.ply) ";
   QString fileName = QFileDialog::getSaveFileName(this,"Write to an ASCII PLY file", "", filter);
   if (fileName.isEmpty())
      return;
   QByteArray ba = fileName.toLocal8Bit();
   char *filenameout = ba.data();
   flag=3;
   m_GLwidget_secondwindow->DataPrint(filenameout);
   ui.infoLabel->setText(tr("Wrote  ")+tr(filenameout));
}


void MainWindow::LinesofCurvature()
{
   int wrote=lioc();  // here as test
   if (wrote == 0) {
       ui.infoLabel->setText(tr("gnuplot called successfully  ")); }
   else {
       ui.infoLabel->setText(tr("gnuplot call failed!"));}
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


void MainWindow::about()
{
   // https://www.modernescpp.com/index.php/asynchronous-callable-wrappers
   static const unsigned int hwGuess= 4;
   unsigned int hw = std::thread::hardware_concurrency();
   unsigned int hwConcurr= (hw != 0)? hw : hwGuess;
   std::string t = std::to_string(hwConcurr);
   char const *n_char = t.c_str();
   //ui.infoLabel->setText(tr("CPU Cores found by Kernunos: ")+n_char);

   ui.infoLabel->setText(tr("Invoked <b>Help|About</b>"));
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

void MainWindow::createActions()
{

   openAct = new QAction(tr("&Open..."), this);
   openAct->setShortcuts(QKeySequence::Open);
   openAct->setStatusTip(tr("Open an existing file"));
   connect(openAct, &QAction::triggered, this, &MainWindow::open);

   compareAct = new QAction(tr("&Compare..."), this);
   compareAct->setStatusTip(tr("Compare to previous file"));
   connect(compareAct, &QAction::triggered, this, &MainWindow::compare);

   zernAct = new QAction(tr("&Compute Zernike Coefficients"), this);
   zernAct->setStatusTip(tr("Save the document to disk"));
   connect(zernAct, &QAction::triggered, this, &MainWindow::zern);

   AddNewAct = new QAction(tr("&New"), this);
   AddNewAct->setShortcuts(QKeySequence::New);
   AddNewAct->setStatusTip(tr("New Window"));
   connect(AddNewAct, &QAction::triggered, this, &MainWindow::onAddNew);

   ply2binAct = new QAction(tr("Binary PLY format..."), this);
   ply2binAct->setStatusTip(tr("Write a binary PLY file"));
   connect(ply2binAct, &QAction::triggered, this, &MainWindow::ply2bin);

   off2stlAct = new QAction(tr("STL (ASCII or binary) format..."), this);
   off2stlAct->setStatusTip(tr("Write an ASCII or binary STL file"));
   connect(off2stlAct, &QAction::triggered, this, &MainWindow::off2stl);

   makeoffAct = new QAction(tr("OFF format..."), this);
   makeoffAct->setStatusTip(tr("Write an OFF file"));
   connect(makeoffAct, &QAction::triggered, this, &MainWindow::makeoff);

   makeplyAct = new QAction(tr("PLY (ASCII) format..."), this);
   makeplyAct->setStatusTip(tr("Write an ASCII PLY file"));
   connect(makeplyAct, &QAction::triggered, this, &MainWindow::makeply);

   importexportAct = new QAction(tr("Export with Assimp (multiple formats)... "), this);
   importexportAct->setStatusTip(tr("Export with Assimp (multiple formats)... "));
   connect(importexportAct, &QAction::triggered, this, &MainWindow::importexport);

   exitAct = new QAction(tr("E&xit"), this);
   exitAct->setShortcuts(QKeySequence::Quit);
   exitAct->setStatusTip(tr("Exit the application"));
 //  connect(exitAct, &QAction::triggered, this, &QWidget::close);
   connect(exitAct, &QAction::triggered, qApp, &QApplication::closeAllWindows);

   aboutAct = new QAction(tr("&About"), this);
   aboutAct->setStatusTip(tr("Show the application's About box"));
   connect(aboutAct, &QAction::triggered, this, &MainWindow::about);

   lightAct = new QAction(tr("&Lighting"), this);
   lightAct->setStatusTip(tr("Change the lighting in the window"));
   connect(lightAct, &QAction::triggered, this, &MainWindow::light);
   lightAct->setCheckable(true);
   lightAct->setChecked(GLwidget::isLight());

   normalAct = new QAction(tr("&Show Normals"), this);
   normalAct->setStatusTip(tr("Show the surface normals"));
   connect(normalAct, &QAction::triggered, this, &MainWindow::normal);
   normalAct->setCheckable(true);
   normalAct->setChecked(GLwidget::isNormal());

   liocAct = new QAction(tr("&Lines of Curvature"), this);
   liocAct->setStatusTip(tr("Show plot of lines of curvature"));
   connect(liocAct, &QAction::triggered, this, &MainWindow::LinesofCurvature);

   aboutQtAct = new QAction(tr("About &Qt"), this);
   aboutQtAct->setStatusTip(tr("Show the Qt library's About box"));
   connect(aboutQtAct, &QAction::triggered, qApp, &QApplication::aboutQt);
   connect(aboutQtAct, &QAction::triggered, this, &MainWindow::aboutQt);

}

void MainWindow::createMenus()
{

   fileMenu = menuBar()->addMenu(tr("&File"));
   fileMenu->addAction(openAct);
   fileMenu->addAction(compareAct);
   fileMenu->addAction(AddNewAct);
   exportMenu = fileMenu->addMenu(tr("&Export"));
   exportMenu->addAction(makeplyAct);
   exportMenu->addAction(ply2binAct);
   exportMenu->addAction(makeoffAct);
   exportMenu->addAction(off2stlAct);
   exportMenu->addAction(importexportAct);
   fileMenu->addSeparator();
   fileMenu->addAction(exitAct);
   analyzeMenu = menuBar()->addMenu(tr("&Analyze"));
   analyzeMenu->addAction(zernAct);
   analyzeMenu->addAction(liocAct);
   viewMenu = menuBar()->addMenu(tr("&View"));
   viewMenu->addAction(lightAct);
   viewMenu->addAction(normalAct);
   helpMenu = menuBar()->addMenu(tr("&Help"));
   helpMenu->addAction(aboutAct);
   helpMenu->addAction(aboutQtAct);
}

void MainWindow::updateResult()
{
   const int sum = ui.inputSpinBox1->value() +  ui.inputSpinBox2->value();
   ui.outputWidget->setText(QString::number(sum));
   ui.progressBar->setValue(sum);
}

void MainWindow::onAddNew()
{
    if (!centralWidget()){
       setCentralWidget(new Window);
/*   if (!centralWidget()){
       QGraphicsScene *scene2 = new QGraphicsScene();
       ui.graphicsView_2->setScene(scene2);
       scene2->addWidget(new Window);
*/
   }
   else
       QMessageBox::information(this, tr("Cannot Add New Window"),
                                tr("Already occupied. Undock first."));

   ui.infoLabel->setText(tr("Invoked <b>File|New</b>"));
}

int main(int argc, char *argv[])
{
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

//  logs a comment
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

