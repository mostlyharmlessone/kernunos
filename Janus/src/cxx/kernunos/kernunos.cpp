#include "kernunos.h"
//lifted from examples zoomlinechart
#include "chart.h"   // Copyright (C) 2023 The Qt Company Ltd.
#include "chartview.h" // SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause
#include "window.h"
#include <QSlider>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QPushButton>
#include <QApplication>
#include <QMessageBox>
#include <QMainWindow>
#include <iostream>

// global settings
const unsigned int SCR_WIDTH = 1500;
const unsigned int SCR_HEIGHT = 800;

int flag=0;

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

void MainWindow::open()
{
   ui.infoLabel->setText(tr("Invoked <b>File|Open</b>"));

   QString filter = "All (*.*);;PentaCam (*.CUR *.ELE *.CUR.CSV *.ELE.CSV);;EyeSys (*.DAT);;Atlas (*.CSV)";
   QString fileName = QFileDialog::getOpenFileName(this,"Open a file", "", filter);
   if (fileName.isEmpty())
       return;
   QByteArray ba = fileName.toLocal8Bit();
   const char *filename = ba.data();
   ui.infoLabel->setText(tr("filename:  ")+tr(filename));
   if (!fileName.isEmpty())
       m_GLwidget->DataLoad(fileName, false);
   update();
}

void MainWindow::compare()
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

void MainWindow::save()
{
   ui.infoLabel->setText(tr("Invoked <b>File|Save</b>"));
}

void MainWindow::print()
{
   ui.infoLabel->setText(tr("Invoked <b>File|Print</b>"));
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
   //  these could come in handy later for available number of threads/cores for asynchronous tasks
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

   saveAct = new QAction(tr("&Save"), this);
   saveAct->setShortcuts(QKeySequence::Save);
   saveAct->setStatusTip(tr("Save the document to disk"));
   connect(saveAct, &QAction::triggered, this, &MainWindow::save);

   AddNewAct = new QAction(tr("&New"), this);
   AddNewAct->setShortcuts(QKeySequence::New);
   AddNewAct->setStatusTip(tr("New Window"));
   connect(AddNewAct, &QAction::triggered, this, &MainWindow::onAddNew);

   printAct = new QAction(tr("&Print..."), this);
   printAct->setShortcuts(QKeySequence::Print);
   printAct->setStatusTip(tr("Print the document"));
   connect(printAct, &QAction::triggered, this, &MainWindow::print);

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

   normalAct = new QAction(tr("&Show Normals"), this);
   lightAct->setStatusTip(tr("Show the surface normals"));
   connect(normalAct, &QAction::triggered, this, &MainWindow::normal);

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
   fileMenu->addAction(saveAct);
   fileMenu->addAction(AddNewAct);
   fileMenu->addAction(printAct);
   fileMenu->addSeparator();
   fileMenu->addAction(exitAct);
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

void MainWindow::loadFile(QString& fileName)
{
   if (fileName.isEmpty())
       return;
   QByteArray ba = fileName.toLocal8Bit();
   const char *filename = ba.data();
   ui.infoLabel->setText(tr("filename:  ")+tr(filename));
   if (!fileName.isEmpty())
       m_GLwidget->DataLoad(fileName, false);
   update();
}

int main(int argc, char *argv[])
{
   QApplication app(argc, argv);
   QCoreApplication::setOrganizationName("QtProject");
   QCoreApplication::setApplicationName("kernunos front end");
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
     window.loadFile(parser.positionalArguments().first());

    GLwidget::setNormal(parser.isSet(normalsOption));
    GLwidget::setTransparent(parser.isSet(transparentOption));
    if (GLwidget::isTransparent()) {
        window.setAttribute(Qt::WA_TranslucentBackground);
        window.setAttribute(Qt::WA_NoSystemBackground, false);
    }

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
}

