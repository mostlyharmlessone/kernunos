#ifndef GLWIDGET_H
#define GLWIDGET_H

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

QT_BEGIN_NAMESPACE
class QAction;
class QActionGroup;
class QLabel;
class QMenu;
QT_END_NAMESPACE

class MainWindow ;

class GLwidget : public QOpenGLWidget, protected QOpenGLFunctions
{
    Q_OBJECT

  public:
    GLwidget( QWidget *parent );
    ~GLwidget();  

    bool DataLoad(QString fileName, bool first);


  public slots:

  signals:

  protected:

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
    int timerID;

    int oldX, oldY;
    int newX, newY;

    bool loaded;
    bool rotate;
    bool useArcBall;
};


#endif // GLWIDGET_H
