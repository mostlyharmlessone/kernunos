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
    QMenu *fileMenu;
    QMenu *analyzeMenu;
    QMenu *exportMenu;
    QMenu *viewMenu;
    QMenu *helpMenu;
    QAction *openAct;
    QAction *compareAct;
    QAction *zernAct;
    QAction *AddNewAct;
    QAction *exitAct;
    QAction *ply2binAct;
    QAction *off2stlAct;
    QAction *makeoffAct;
    QAction *makeplyAct;
    QAction *importexportAct;
    QAction *lightAct;
    QAction *liocAct;
    QAction *normalAct;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QLabel *infoLabel;

};

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)




#endif // KERNUNOS_H
