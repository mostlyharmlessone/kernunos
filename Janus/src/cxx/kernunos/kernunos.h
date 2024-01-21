#ifndef KERNUNOS_H
#define KERNUNOS_H

#include <cmath>
#include <QtMath>
#include "GLwidget.h"
#include "ui_mainwindow.h"
#include <QMainWindow>

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

private:
    void createActions();
    void createMenus();
    Ui::MainWindow ui;

    GLwidget* m_GLwidget;
    GLwidget* m_GLwidget_secondwindow;
    QMenu *fileMenu;
    QMenu *helpMenu;
    QAction *openAct;
    QAction *compareAct;
    QAction *saveAct;
    QAction *exitAct;
    QAction *printAct;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QLabel *infoLabel;
};

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)




#endif // KERNUNOS_H
