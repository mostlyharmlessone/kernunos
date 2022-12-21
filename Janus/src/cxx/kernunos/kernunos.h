#ifndef KERNUNOS_H
#define KERNUNOS_H

#include <cmath>
#include <QtMath>

// Include standard headers
#include <QtWidgets>
#include <QApplication>
#include <QMouseEvent>
#include <QOpenGLShaderProgram>
#include <QCoreApplication>
#include <math.h>
#include <stdio.h>
#include <chrono>
#include <iostream>
#include <future>
#include <thread>
#include <memory>
#include <QMainWindow>
#include <QOpenGLWidget>
#include <QOpenGLShaderProgram>
#include <QOpenGLFunctions>
#include <QOpenGLTexture>
#include <QKeyEvent>
#include <QTime>
#include <QVector3D>
#include <QMatrix4x4>
#include <QPointF>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLBuffer>

#include <QCommandLineParser>
#include <QCommandLineOption>
#include <QLocale>
#include <QTranslator>

#include <qt6/QtCore/qtmetamacros.h>

QT_BEGIN_NAMESPACE
class QAction;
class QActionGroup;
class QLabel;
class QMenu;
QT_END_NAMESPACE


class GLwidget;

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

private:
    void createActions();
    void createMenus();

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

class GLwidget : public QOpenGLWidget, protected QOpenGLFunctions
{
    Q_OBJECT

  public:
    GLwidget( QWidget *parent=nullptr );
    ~GLwidget();

    bool DataLoad(QString fileName);

//  public slots:

//  signals:

//  protected:

    void initializeGL(void);
    void resizeGL( int w, int h );
    void paintGL();
    void keyPressEvent( QKeyEvent *e);
    void mouseMoveEvent(QMouseEvent *e);
    void mousePressEvent(QMouseEvent *e);
    void mouseReleaseEvent(QMouseEvent *e);
    void wheelEvent(QWheelEvent *e);
    void timerEvent(QTimerEvent*);
    void updateMouse();

  private:
    MainWindow *m_parent;
    QOpenGLShaderProgram shaderProgram;
    GLuint programID;
    QMatrix4x4 mModelMatrix;
    QMatrix4x4 mRotate;
    QMatrix4x4 mViewMatrix;
    QMatrix4x4 mProjectionMatrix;

    bool LoadSurfaceToBuffer(int nV, int nE, GLfloat *vertices, GLuint *elements);
    QVector3D getArcBallVector(int x, int y);

    QVector3D cameraPos;
    QVector3D mPosition;
    GLuint MatrixID;

    GLuint elementbuffer;
    GLuint vertexbuffer;

    int mWidth;
    int mHeight;

    int oldX, oldY;
    int newX, newY;

    bool rotate;
    bool useArcBall;
};


#endif // KERNUNOS_H
