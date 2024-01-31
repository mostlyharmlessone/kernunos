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

extern bool success;
extern bool paintme;

//how very Fortran that these need to be static & global
extern int nV;
extern int nE;
extern std::vector<GLuint> Elements;
extern std::vector<GLfloat> Vertices;
extern GLfloat* vertices;
extern GLuint* elements;

extern "C" {
void janus_(int *flag, const char *filename, GLuint *elements, GLfloat *vertices, int *nV, int *nE); // needs an underscore despite c_interface.f90 bind C declaration
};

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

protected:

private slots:
    void open();
    void compare();
    void save();
    void print();
    void about();
    void aboutQt();
    void updateResult();
    void onAddNew();
    void dockUndock();

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
    QMenu *helpMenu;
    QAction *openAct;
    QAction *compareAct;
    QAction *saveAct;
    QAction *AddNewAct;
    QAction *exitAct;
    QAction *printAct;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QLabel *infoLabel;
};

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)




#endif // KERNUNOS_H
