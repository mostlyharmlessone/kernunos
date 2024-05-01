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

extern "C" {
void ConvertOFFtoSTL_C_(char *iname, char *oname);
};

// calling C code
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
    void compare();
    void zern();
    void ply2bin();
    void off2stl();
    void makeoff();
    void makeply();
    void importexport();
    void normal();
    void light();
    void about();
    void aboutQt();
    void updateResult();
    void LinesofCurvature();
//    void functionmap();

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

    QMenu *changemapMenu;
    QAction *AxialAct;
    QAction *TangentialAct;
    QAction *InstantaneousAct;
    QAction *MeanAct;
    QAction *AstigAct;
    QAction *ElevationAct;
    QAction *Z44VerticalQuatrafoilAct;
    QAction *Z42Vertical2ndAstig;
    QAction *Z40SphericalAberration;
    QAction *Z4neg2Oblique2ndAstig;
    QAction *Z4neg4ObliqueQuatrafoilAct;
    QAction *Z33ObliqueTrefoilAct;
    QAction *Z3neg3VerticalTrefoilAct;
    QAction *Z31HorizontalComaAct;
    QAction *Z3neg1VerticalComaAct;
    QAction *Z22VerticalAstig;
    QAction *Z2neg2ObliqueAstig;
    QAction *Z20Defocus;
    QAction *Z11Xtilt;
    QAction *Z1neg1Ytilt;
    QAction *Z00Piston;
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
    QAction *lightAct;
    QAction *normalAct;

    QMenu *analyzeMenu;
    QAction *zernAct;
    QAction *liocAct;

    QMenu *helpMenu;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QLabel *infoLabel;

};

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)




#endif // KERNUNOS_H
