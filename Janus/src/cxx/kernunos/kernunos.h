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

#include "mainwindow.h"

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)

class kernunos : public QOpenGLWidget, protected QOpenGLFunctions
{
    Q_OBJECT

  public:
    kernunos( QWidget *parent=nullptr );

    ~kernunos();
    QVector3D getArcBallVector(int x, int y);
    bool DataLoad(QString fileName);
    void LoadSurfaceToBuffer(int nV, int nE, GLfloat* vertices, GLuint* elements);

  public slots:

  signals:

  protected:
    void initializeGL(void);
    void resizeGL( int w, int h );
    void paintGL();
    void keyPressEvent( QKeyEvent *e);
    void mouseMoveEvent(QMouseEvent* e);
    void mousePressEvent(QMouseEvent* e);
    void mouseReleaseEvent(QMouseEvent *e);
    void wheelEvent(QWheelEvent *e);
    void timerEvent(QTimerEvent*);
    void updateMouse();


  private:
    MainWindow*  m_parent;
    QOpenGLShaderProgram shaderProgram;
    GLuint programID;
    QMatrix4x4 mModelMatrix;
    QMatrix4x4 mRotate;
    QMatrix4x4 mViewMatrix;
    QMatrix4x4 mProjectionMatrix;

    QVector3D cameraPos;
    QVector3D mPosition;
    GLuint MatrixID;
    GLuint arraybuffer;
    GLuint elementbuffer;
    GLuint vertexbuffer;

    int nV=34560;
    int nE=26130;
    GLfloat* vertices;
    GLuint* elements;

    int mWidth;
    int mHeight;

    int oldX, oldY;
    int newX, newY;

    bool success;
    bool rotate;
    bool useArcBall;
};


#endif // KERNUNOS_H
