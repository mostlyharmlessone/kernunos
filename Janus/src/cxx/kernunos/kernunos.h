#ifndef KERNUNOS_H
#define KERNUNOS_H

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


class kernunos : public QOpenGLWidget, protected QOpenGLFunctions
{
  public:
    kernunos( QWidget *parent=0 );
    ~kernunos();

    QVector3D getArcBallVector(int x, int y);

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

    QOpenGLShaderProgram shaderProgram;
    GLuint programID;
    QMatrix4x4 mModelMatrix;
    QMatrix4x4 mRotate;
    QMatrix4x4 mViewMatrix;
    QMatrix4x4 mProjectionMatrix;

    QVector3D cameraPos;
    QVector3D mPosition;
    QOpenGLTexture* mTexture;
    GLuint MatrixID;
    GLuint vertexPosition_modelspaceID;
    GLuint vertexUVID;
    GLuint vertexbuffer;
    GLuint uvbuffer;

    int mWidth;
    int mHeight;

    int oldX, oldY;
    int newX, newY;

    bool rotate;
    bool useArcBall;
};


class MainWindow : public QMainWindow
{

public:
    MainWindow(QWidget *parent=nullptr);
    ~MainWindow();

protected:

private slots:

private:

    kernunos *GLWidget;

};


const GLchar* vertexSource = R"glsl(
#version 400 core
layout (location = 0) in vec3 position;   // the position variable has attribute position 0
layout (location = 1) in vec3 incolor; // the color variable has attribute position 1

out vec3 outColor; // output a color to the fragment shader

uniform mat4 mMVP;

void main()
{
    gl_Position = mMVP * vec4(position, 1.0);
    outColor = incolor; // set outColor to the input color we got from the vertex data
}
)glsl";

const GLchar* fragmentSource = R"glsl(
#version 400 core
out vec4 fragColor;
in vec3 outColor;

void main()
{
    fragColor = vec4(outColor, 1.0);
}
)glsl";

const GLchar* TransformFragmentShader = R"glsl(
#version 120

// Interpolated values from the vertex shaders
varying vec2 UV;

// Ouput data
//out vec3 color;

// Values that stay constant for the whole mesh.
uniform sampler2D myTextureSampler;

void main(){

        // Output color = color of the texture at the specified UV
        gl_FragColor = texture2D( myTextureSampler, UV );
}
)glsl";

const GLchar* TransformVertexShader = R"glsl(
#version 120

// Input vertex data, different for all executions of this shader.
attribute vec3 vertexPosition_modelspace;
attribute vec2 vertexUV;

// Output data ; will be interpolated for each fragment.
varying vec2 UV;

// Values that stay constant for the whole mesh.
uniform mat4 MVP;

void main(){

    // Output position of the vertex, in clip space : MVP * position
    gl_Position =  MVP * vec4(vertexPosition_modelspace,1);

    // UV of the vertex. No special space for this one.
    UV = vertexUV;
}
)glsl";


#endif // KERNUNOS_H
