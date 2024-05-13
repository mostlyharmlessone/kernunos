#ifndef KERNUNOS_H
#define KERNUNOS_H

#include "GLwidget.h"
#include "ui_mainwindow.h"
#include <QWidget>

QT_FORWARD_DECLARE_CLASS(QSlider)
QT_FORWARD_DECLARE_CLASS(QPushButton)

// global variables
extern const unsigned int SCR_WIDTH;
extern const unsigned int SCR_HEIGHT;

extern int flag;
extern char *filename;

extern bool success;
extern bool paintme;

//how very Fortran that these need to be static & global
extern int nV;
extern int nE;
extern std::vector<GLuint> Elements;
extern std::vector<GLfloat> Vertices;
extern GLfloat* vertices;
extern GLuint* elements;

// calling fortran code
extern "C" {
void janus_(int *flag, char *filename, GLuint *elements, GLfloat *vertices, int *nV, int *nE);
};

// calling C code
extern "C" {
void ConvertOFFtoSTL_C_(char *iname, char *oname);
};

extern "C" {
int ConvertPLYtoBIN(const char *iname, const char *oname);
};

extern "C" {
void LogC(const char *Message);
};

extern "C" {
void Ccounter(int *inc);
};

// external cpp code
int lioc();

extern QString *m_GLString;
extern QString glstring_global;

QT_BEGIN_NAMESPACE
class QAction;
class QActionGroup;
class QLabel;
class QMenu;
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();
    void SetGLString(QString& gls);
    void loadFile(QString& fileName, bool filepresent);
    QTimer t;
//    static bool isNormal() { return m_normal; }
//    static void setNormal(bool t) { m_normal = t; }

protected:

private slots:
    void open();
    void redraw();
    void compare();
    void zern();
    void ply2bin();
    void off2stl();
    void makeoff();
    void makeply();
    void importexport();
    void normal();
    void checkmapsflags();
    void checkfctsflags();
    void light();
    void about();
    void aboutQt();
    void updateResult();
    void LinesofCurvature();
    void center();
    void gnuplotsplot();
    void fctAxial();
    void fctTangential();
    void fctInstantaneous();
    void fctMean();
    void fctMongeAstig();
    void fctElevation();
    void fctZ44();
    void fctZ42();
    void fctZ40();
    void fctZ4neg2();
    void fctZ4neg4();
    void fctZ33();
    void fctZ31();
    void fctZ3neg1();
    void fctZ3neg3();
    void fctZ22();
    void fctZ20();
    void fctZ2neg2();
    void fctZ11();
    void fctZ1neg1();
    void fctZ00();
    void tweakcenterNode();
    void tweakadjustradii();
    void tweakcubic();
    void tweakLSQfill();
    void tweakSplinefill();
    void colorrgb2();
    void colorrgb5();
    void colorhsbrgb();
    void colorgplotpalette();
    void colorPerceptualUniformfixed();
    void colorPerceptualUniformpalette();
    void colorUSSpalettefixed();
    void colorUSSpalette();

private:
    void createActions();
    void createMenus();
    Ui::MainWindow ui;

    void dock();
    void undock();
    QSlider *createSlider();

    GLwidget *glWidget;
    QSlider *xSlider;
    QSlider *ySlider;
    QSlider *zSlider;
    QPushButton *dockBtn;

    GLwidget* m_GLwidget;
    GLwidget* m_GLwidget_secondwindow;

    QMenu *functionMenu;
    QAction *AxialAct;
    QAction *TangentialAct;
    QAction *InstantaneousAct;
    QAction *MeanAct;
    QAction *AstigAct;
    QAction *ElevationAct;
    QAction *Z44VerticalQuatrafoilAct;
    QAction *Z42Vertical2ndAstigAct;
    QAction *Z40SphericalAberrationAct;
    QAction *Z4neg2Oblique2ndAstigAct;
    QAction *Z4neg4ObliqueQuatrafoilAct;
    QAction *Z33ObliqueTrefoilAct;
    QAction *Z3neg3VerticalTrefoilAct;
    QAction *Z31HorizontalComaAct;
    QAction *Z3neg1VerticalComaAct;
    QAction *Z22VerticalAstigAct;
    QAction *Z2neg2ObliqueAstigAct;
    QAction *Z20DefocusAct;
    QAction *Z11XtiltAct;
    QAction *Z1neg1YtiltAct;
    QAction *Z00PistonAct;

    QMenu *colorMenu;
    QAction *rgb2Act;
    QAction *rgb5Act;
    QAction *hsbrgbAct;
    QAction *gplotpaletteAct;
    QAction *USSfixedAct;
    QAction *USSPaletteAct;
    QAction *PerceptualfixedAct;
    QAction *PerceptuallyUniformPaletteAct;

    QMenu *fileMenu;
    QAction *openAct;
    QAction *compareAct;
    QAction *exitAct;

    QMenu *exportMenu;
    QAction *ply2binAct;
    QAction *off2stlAct;
    QAction *makeoffAct;
    QAction *makeplyAct;
    QAction *importexportAct;

    QMenu *viewMenu;
    QAction *redrawAct;
    QAction *lightAct;
    QAction *normalAct;

    QMenu *analyzeMenu;
    QAction *zernAct;
    QAction *liocAct;
    QAction *centerAct;
    QAction *gnuplotAct;

    QMenu *tweaksMenu;
    QAction *centernodeAct;
    QAction *adjustradiiAct;
    QAction *cubicAct;
    QAction *LSQfillinAct;
    QAction *SplinefillinAct;

    QMenu *helpMenu;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QAction *HelpAct;
    QLabel *infoLabel;

};

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)




#endif // KERNUNOS_H
