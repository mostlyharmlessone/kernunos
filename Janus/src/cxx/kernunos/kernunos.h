#ifndef GLwidget_H
#define GLwidget_H

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

class GLShaders: public QOpenGLWidget, protected QOpenGLFunctions
{
public:
    GLShaders();
    ~GLShaders();

    void Init();
    bool Use();
    void StopUse();
    void CleanUp();

    GLuint GetAttribLoc(const std::string& name);
    GLuint GetUnifLoc(const std::string& name);

private:
    GLuint programID;
};

class GLTriangles: public QOpenGLWidget, protected QOpenGLFunctions
{
public:
    GLTriangles();
    ~GLTriangles();

    void Clear();
    void SetBuffers(GLShaders* theShader, int nV, int nE,
                    GLfloat* vertices, GLuint* elements);
    void Draw();

private:
    GLuint vertexbuffer;
    GLuint elementbuffer;
    GLShaders* m_triangShaders;
};

class GLManager: public QOpenGLWidget, protected QOpenGLFunctions
{
public:
     GLManager();
    ~GLManager();

    const GLubyte* GetGLVersion();
    const GLubyte* GetGLVendor();
    const GLubyte* GetGLRenderer();

    void SetShadersAndTriangles();
    void SetViewport(int x, int y, int width, int height);
    void Render();

private:
    GLShaders   m_TriangShaders;
    GLTriangles m_Triangles;

};

class GLwidget;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();
    void SetGLString(QString& gls)
        {m_GLString =  gls; }

protected:

private slots:
    void open();
    void save();
    void print();
    void about();
    void aboutQt();

private:
    void createActions();
    void createMenus();

    QString m_GLString;

    GLwidget* m_GLwidget;
    QMenu *fileMenu;
    QMenu *helpMenu;
    QAction *openAct;
    QAction *saveAct;
    QAction *printAct;
    QAction *exitAct;
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
    QVector3D getArcBallVector(int x, int y);

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
    GLManager* m_oglManager;
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


#endif // GLwidget_H
