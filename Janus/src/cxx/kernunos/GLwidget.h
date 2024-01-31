#ifndef GLWIDGET_H
#define GLWIDGET_H

// Include standard headers
#include <QtWidgets>
#include <QApplication>
#include <QMouseEvent>
#include <QCoreApplication>
#include <QMainWindow>
#include <QKeySequence>
#include <QMenuBar>
#include <QMenu>
#include <QMessageBox>

#include <cmath>
#include <math.h>
#include <stdio.h>
#include <chrono>
#include <iostream>
#include <future>
#include <thread>
#include <vector>
#include <memory>

#include <QOpenGLWidget>
#include <QOpenGLShaderProgram>
#include <QOpenGLFunctions>
#include <QOpenGLTexture>
#include <QKeyEvent>
#include <QTime>
#include <QtMath>
#include <QVector3D>
#include <QMatrix4x4>
#include <QPointF>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLBuffer>

#include <QtCharts/QChartView>
#include <QtCharts/QBarSeries>
#include <QtCharts/QBarSet>
#include <QtCharts/QLegend>
#include <QtCharts/QBarCategoryAxis>
#include <QtCharts/QHorizontalStackedBarSeries>
#include <QtCharts/QValueAxis>


#include <QCommandLineParser>
#include <QCommandLineOption>
#include <QLocale>
#include <QTranslator>

#include <qt6/QtCore/qtmetamacros.h>

#include "logo.h"

// global variables
extern const unsigned int SCR_WIDTH;
extern const unsigned int SCR_HEIGHT;

extern int flag;

extern bool success;
extern bool paintme;

//how very FORTRAN that these need to be static & global
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

class MainWindow ;

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)

class GLwidget : public QOpenGLWidget, protected QOpenGLFunctions
{
    Q_OBJECT

  public:
    GLwidget( QWidget *parent = nullptr );
    ~GLwidget();

    static bool isTransparent() { return m_transparent; }
    static void setTransparent(bool t) { m_transparent = t; }

    QSize minimumSizeHint() const override;
    QSize sizeHint() const override;

    bool DataLoad(QString fileName, bool first);


  public slots:

    void setXRotation(int angle);
    void setYRotation(int angle);
    void setZRotation(int angle);
    void cleanup();


  signals:

    void xRotationChanged(int angle);
    void yRotationChanged(int angle);
    void zRotationChanged(int angle);


  protected:

    void initializeGL(void) override;
    void resizeGL( int w, int h ) override;
    void paintGL() override;
    void keyPressEvent( QKeyEvent *e) override;
    void mouseMoveEvent(QMouseEvent *e) override;
    void mousePressEvent(QMouseEvent *e) override;
   // void mouseReleaseEvent(QMouseEvent *e) override;
    void wheelEvent(QWheelEvent *e) override;
    void timerEvent(QTimerEvent*) override;
   // void updateMouse();

  private:

    MainWindow *m_parent;
   // QOpenGLShaderProgram shaderProgram;
    QOpenGLShaderProgram *shaderProgram = nullptr;
    GLuint programID;
    QMatrix4x4 mModelMatrix;
    //QMatrix4x4 mRotate;
    QMatrix4x4 mViewMatrix;
    QMatrix4x4 mProjectionMatrix;

    bool LoadSurfaceToBuffer(int nV, int nE, GLfloat *vertices, GLuint *elements);
    //QVector3D getArcBallVector(int x, int y);

    QVector3D cameraPos;
    QVector3D mPosition;
    GLuint MatrixID;

    GLuint elementbuffer;
    GLuint vertexbuffer;

    //int mWidth;
    //int mHeight;
    int timerID;

    //int oldX, oldY;
    //int newX, newY;

    bool loaded;
    bool rotate;
    //bool useArcBall;

    void setupVertexAttribs();

    bool m_core;
    int m_xRot = 0;
    int m_yRot = 0;
    int m_zRot = 0;
    QPoint m_lastPos;
    Logo m_logo;
    QOpenGLVertexArrayObject m_vao;
    QOpenGLBuffer m_logoVbo;
    QOpenGLShaderProgram *m_program = nullptr;
    int m_projMatrixLoc = 0;
    int m_mvMatrixLoc = 0;
    int m_normalMatrixLoc = 0;
    int m_lightPosLoc = 0;
    QMatrix4x4 m_proj;
    QMatrix4x4 m_camera;
    QMatrix4x4 m_world;
    static bool m_transparent;


};


#endif // GLWIDGET_H
